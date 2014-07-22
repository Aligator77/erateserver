-module(erateserver_subproto).

-behavior(cowboy_http_handler).
-export([init/3, handle/2, terminate/3]).

-behavior(cowboy_sub_protocol).
-export([upgrade/4]).

-export([receiver/1, responder/1]).

-export([client_connect/3, acquire/3, respond/3, sync_respond/3, handle_message/1]).
-export([send_ping/1]).
-export([send_acquire_request/3, send_acquire_request/4]).

-export([strip_pid/1, restore_pid/1, strip_ref/1, restore_ref/1]).

-define(PROTO, <<"erateserver">>).
-define(SOCK_OPTS, [{packet, 2}, {mode, binary}, {delay_send, true}, {nodelay, true}, {sndbuf, 16384}, {recbuf, 16384}, {send_timeout, 500}, {send_timeout_close, true}]).

init(_, _, _) ->
    {upgrade, protocol, ?MODULE}.

handle(Req, State) ->
    {ok, Req2} = cowboy_req:reply(514, [], [], Req),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.


upgrade(Req, _Env, ?MODULE, Group) when is_atom(Group) ->
    % Check connection: upgrade
	{ok, ConnTokens, Req2} = cowboy_req:parse_header(<<"connection">>, Req),
	true = lists:member(<<"upgrade">>, ConnTokens),
    % Check upgrade: erateserver
	{ok, [?PROTO], Req3} = cowboy_req:parse_header(<<"upgrade">>, Req2),
    do_upgrade(Req3, Group).

-define(ACK_EVERY, 500).
-define(ACK_AFTER, 300).

-record(rcv, {
        group,
        transport, socket,
        responder, respond_path,
        ack_ref, ack_after = ?ACK_EVERY
        }).

-record(rsp, {
        receiver,
        respond_path,
        ack_ref, ack_after = ?ACK_AFTER
        }).


do_upgrade(Req, Group) ->
    % Send upgrade to the client
    {ok, _Req2} = cowboy_req:upgrade_reply(101, [{<<"upgrade">>, ?PROTO}], Req),
    % Ensure upgrade is sent
	receive {cowboy_req, resp_sent} -> ok after 1000 -> erlang:error(no_response_ack) end,

	[Socket, Transport] = cowboy_req:get([socket, transport], Req),
    Transport:setopts(Socket, [{active, true} | ?SOCK_OPTS]),

    RespondPath = {Transport, Socket, slots_to_millis(1, Group)},
    Rsp = #rsp{receiver=self(), respond_path=RespondPath},
    Responder = proc_lib:spawn_opt(?MODULE, responder, [Rsp], [link]),
    Rcv = #rcv{group=Group, transport=Transport, socket=Socket, responder=Responder, respond_path = RespondPath},
    receiver(Rcv).


handle_rcv_mql(MQL, #rcv{} = Rcv) when MQL > 5 ->
    Rcv;
handle_rcv_mql(_MQL, #rcv{transport=Transport, socket=Socket} = Rcv) ->
    Transport:setopts(Socket, [{active, 50}]),
    Rcv.

handle_rcv_data(<<0:16/integer, 1:16/integer, Ref/binary>>, #rcv{responder = Responder} = Rcv) -> % Ping request
    Responder ! {pong, Ref},
    Rcv;
handle_rcv_data(<<NameLen:16/integer, CounterName:NameLen/binary, MaxWait:32/integer, Ref/binary>>, #rcv{group = Group, responder = Responder, ack_after = AA} = Rcv) ->
    %erater:local_async_acquire(Group, CounterName, MaxWait, {mfa, ?MODULE, respond, [Ref, RespondPath]}); % direct socket write
    %erater:local_async_acquire(Group, CounterName, MaxWait, {mfa, ?MODULE, sync_respond, [Ref, Responder]}); % counter waits for response to be sent
    erater:local_async_acquire(Group, CounterName, MaxWait, {Responder, Ref}),
    Rcv#rcv{ack_after = AA - 1}.

rq_new_ack(#rcv{responder = Responder} = Rcv) ->
    NewRef = make_ref(), % os:timestamp(),
    Responder ! {ack_request, NewRef},
    Rcv#rcv{ack_after = ?ACK_EVERY, ack_ref = NewRef}.

receiver(#rcv{ack_after = AA, ack_ref = Ref} = Rcv) when AA =< 0 ->
    receive
        {responder_ack, Ref} ->
            receiver(rq_new_ack(Rcv))
    after
        2000 ->
            exit(no_responses)
    end;
receiver(#rcv{ack_ref = Ref, socket = Socket} = Rcv0) ->
    {_, MQL} = process_info(self(), message_queue_len),
    Rcv = handle_rcv_mql(MQL, Rcv0),
    Rcv1 = receive
        {responder_ack, Ref} ->
            rq_new_ack(Rcv);
        {_Proto, Socket, Data} ->
            handle_rcv_data(Data, Rcv);
        Msg ->
            lager:notice("receiver ~p got unexpected message ~p", [Rcv, Msg]),
            Rcv
    end,
    ?MODULE:receiver(Rcv1).


respond(pong, Ref, {Transport, Socket, _SlotMillis}) ->
    Transport:send(Socket, <<16#81:8, Ref/binary>>);
respond({ok, SlotsWait}, Ref, {Transport, Socket, SlotMillis}) ->
    WaitMillis = SlotMillis * SlotsWait,
    Transport:send(Socket, <<1:8, WaitMillis:32/integer, Ref/binary>>);
respond({error, overflow}, Ref, {Transport, Socket, _SlotMillis}) ->
    Transport:send(Socket, <<0:8, Ref/binary>>);
respond(Response, Ref, {Transport, Socket, _SlotMillis}) ->
    lager:notice("Unexpected erater response ~p", [Response]),
    Transport:send(Socket, <<16#80:8, Ref/binary>>).

sync_respond(Response, Ref, Responder) ->
    Responder ! {erater_sync_response, self(), Ref, Response},
    receive {ack, Ref} -> ok
    after 1000 -> ok
    end.


responder(#rsp{ack_after = AA, ack_ref = Ref, receiver = Receiver} = Rsp) when AA =< 0, Ref /= ignore ->
    Receiver ! {responder_ack, Ref},
    responder(Rsp#rsp{ack_ref = ignore});
responder(#rsp{respond_path = RespondPath, ack_after = AA} = Rsp) ->
    Rsp1 = receive
        {ack_request, Ref} ->
            Rsp#rsp{ack_ref = Ref, ack_after = ?ACK_AFTER};
        {pong, Ref} ->
            respond(pong, Ref, RespondPath),
            Rsp;
        {erater_response, Ref, Response} when is_binary(Ref) ->
            respond(Response, Ref, RespondPath),
            Rsp#rsp{ack_after = AA - 1};
        {erater_sync_response, Sender, Ref, Response} ->
            respond(Response, Ref, RespondPath),
            Sender ! {ack, Ref},
            Rsp#rsp{ack_after = AA - 1};
        Msg ->
            lager:notice("responder responder unexpected message ~p", [Msg]),
            Rsp
    end,
    ?MODULE:responder(Rsp1).

slots_to_millis(0, _Group) ->
    0;
slots_to_millis(SlotsWait, Group) ->
    RPS = erater_group:get_config(Group, rps),
    SlotMillis = 1000 div RPS,
    SlotMillis * SlotsWait.


client_connect(Host, Port, GroupID) ->
    {ok, Socket} = gen_tcp:connect(Host, Port, [{active, false}, binary, {packet, http_bin}]),
    gen_tcp:send(Socket, [<<"GET /rpc/">>, GroupID, <<" HTTP/1.1\r\nHost: undefined\r\nConnection: upgrade\r\nUpgrade: erateserver\r\n\r\n">>]),
    {ok, {_, _, 101, _}} = gen_tcp:recv(Socket, 0, 5000),
    inet:setopts(Socket, [{packet, httph_bin}]),
    drop_headers(Socket),
    ok = inet:setopts(Socket, [{active, true}|?SOCK_OPTS]),
    {ok, Socket}.

drop_headers(Socket) ->
    case gen_tcp:recv(Socket, 0, 5000) of
        {ok, {http_header, _, _, _, _}} ->
            drop_headers(Socket);
        {ok, http_eoh} ->
            ok
    end.

handle_message(<<16#81:8, BinRef/binary>>) ->
    send_response(pong, BinRef);
handle_message(<<0:8, BinRef/binary>>) ->
    send_response({error, overflow}, BinRef);
handle_message(<<16#80:8, BinRef/binary>>) ->
    send_response({error, unexpected_response}, BinRef);
handle_message(<<1:8, WaitMillis:32/integer, BinRef/binary>>) ->
    send_response({ok, WaitMillis}, BinRef).

send_response(Response, BinRef) ->
    {Pid, Ref} = decode_pidref(BinRef),
    Pid ! {erater_response, Ref, Response}.

acquire(Socket, CounterName, MaxWait) when is_binary(CounterName) ->
    Ref = send_acquire_request(Socket, CounterName, MaxWait),
    recv_acquire_response(Ref).

send_acquire_request(Socket, CounterName, MaxWait) ->
    Ref = make_ref(),
    send_acquire_request(Socket, CounterName, MaxWait, {self(), Ref}).

send_acquire_request(Socket, CounterName, MaxWait, {Pid, Ref}) ->
    BinRef = encode_pidref(Pid, Ref),
    NameLen = byte_size(CounterName),
    Packet = <<NameLen:16/integer, CounterName/binary, MaxWait:32/integer, BinRef/binary>>,
    ok = gen_tcp:send(Socket, Packet),
    Ref.

recv_acquire_response(Ref) ->
    receive
        {erater_response, Ref, Result} ->
            Result
    after 5000 ->
            error(acquire_timeout)
    end.



send_ping(Socket) ->
    Ref = make_ref(),
    BinRef = encode_pidref(self(), Ref), % term_to_binary({self(), Ref}),
    Packet = <<0:16/integer, 1:16/integer, BinRef/binary>>,
    ok = gen_tcp:send(Socket, Packet),
    Ref.


encode_pidref(Pid, Ref) ->
    BinPid = strip_pid(Pid),
    BinRef = strip_ref(Ref),
    <<BinPid/binary, BinRef/binary>>.

decode_pidref(<<BinPid:9/binary, BinRef/binary>>) ->
    {restore_pid(BinPid), restore_ref(BinRef)}.

strip_pid(Pid) when is_pid(Pid) ->
    <<131, 103, NodeIdSerCrdt/binary>> = term_to_binary(Pid),
    NodeLen = byte_size(NodeIdSerCrdt) - 9,
    <<_:NodeLen/binary, IdSerCr/binary>> = NodeIdSerCrdt,
    IdSerCr.

restore_pid(IdSerCr) ->
    <<131, NodeBin/binary>> = term_to_binary(node()),
    PidBin = <<131, 103, NodeBin/binary, IdSerCr/binary>>,
    binary_to_term(PidBin).

strip_ref(Ref) when is_reference(Ref) ->
    <<131, 114, Len:16, NodeCrId/binary>> = term_to_binary(Ref),
    NodeLen = byte_size(NodeCrId) - Len*4 - 1,
    <<_:NodeLen/binary, CrId/binary>> = NodeCrId,
    CrId.

restore_ref(CrId) ->
    <<131, NodeBin/binary>> = term_to_binary(node()),
    Len = byte_size(CrId) div 4,
    RefBin = <<131, 114, Len:16, NodeBin/binary, CrId/binary>>,
    binary_to_term(RefBin).
