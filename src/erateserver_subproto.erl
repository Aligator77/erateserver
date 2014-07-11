-module(erateserver_subproto).

-behavior(cowboy_http_handler).
-export([init/3, handle/2, terminate/3]).

-behavior(cowboy_sub_protocol).
-export([upgrade/4]).

-export([receiver/5, responder/1]).

-export([client_connect/3, acquire/3, respond/3, sync_respond/3, handle_message/1]).
-export([send_ping/1]).
-export([send_acquire_request/3]). % debug

-define(PROTO, <<"erateserver">>).
-define(SOCK_OPTS, [{packet, 2}, {mode, binary}, {delay_send,true}, {nodelay, true}, {high_watermark, 32768}, {high_msgq_watermark, 32768}, {priority, 4}, {send_timeout, 1000}, {send_timeout_close, true}]).

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

do_upgrade(Req, Group) ->
    % Send upgrade to the client
    {ok, _Req2} = cowboy_req:upgrade_reply(101, [{<<"upgrade">>, ?PROTO}], Req),
    % Ensure upgrade is sent
	receive {cowboy_req, resp_sent} -> ok after 1000 -> erlang:error(no_response_ack) end,

	[Socket, Transport] = cowboy_req:get([socket, transport], Req),
    Transport:setopts(Socket, [{active, true} | ?SOCK_OPTS]),

    RespondPath = {Transport, Socket, slots_to_millis(1, Group)},
    Responder = proc_lib:spawn_opt(?MODULE, responder, [RespondPath], [link]),
    receiver(Transport, Socket, Group, Responder, RespondPath).


receiver(Transport, Socket, Group, Responder, RespondPath) ->
    {_, MQL} = process_info(self(), message_queue_len),
    (MQL =< 5) andalso Transport:setopts(Socket, [{active, 50}]),
    receive
        {_Proto, Socket, <<0:16/integer, 1:16/integer, Ref/binary>>} -> % Ping request
            respond(pong, Ref, RespondPath);
        {_Proto, Socket, <<NameLen:16/integer, CounterName:NameLen/binary, MaxWait:32/integer, Ref/binary>>} ->
            %erater:local_async_acquire(Group, CounterName, MaxWait, {mfa, ?MODULE, respond, [Ref, RespondPath]});
            erater:local_async_acquire(Group, CounterName, MaxWait, {mfa, ?MODULE, sync_respond, [Ref, Responder]});
        Msg ->
            lager:notice("server receiver unexpected message ~p", [Msg]),
            ok
    end,
    receiver(Transport, Socket, Group, Responder, RespondPath).

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

responder(RespondPath) ->
    receive
        {erater_response, Ref, Response} when is_binary(Ref) ->
            respond(Response, Ref, RespondPath);
        {erater_sync_response, Sender, Ref, Response} ->
            respond(Response, Ref, RespondPath),
            Sender ! {ack, Ref};
        Msg ->
            lager:notice("server responder unexpected message ~p", [Msg]),
            ok
    end,
    responder(RespondPath).

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
    {Pid, Ref} = binary_to_term(BinRef),
    Pid ! {erater_response, Ref, Response}.

acquire(Socket, CounterName, MaxWait) when is_binary(CounterName) ->
    Ref = send_acquire_request(Socket, CounterName, MaxWait),
    recv_acquire_response(Ref).

send_acquire_request(Socket, CounterName, MaxWait) ->
    NameLen = byte_size(CounterName),
    Ref = make_ref(),
    BinRef = term_to_binary({self(), Ref}),
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
    BinRef = term_to_binary({self(), Ref}),
    Packet = <<0:16/integer, 1:16/integer, BinRef/binary>>,
    ok = gen_tcp:send(Socket, Packet),
    Ref.
