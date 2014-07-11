-module(erateserver_shard_proxy).
-export([start_link/2]).
-export([acquire/3]).

-behavior(gen_server).
-export([init/1, handle_info/2, handle_cast/2, handle_call/3, code_change/3, terminate/2]).

regname(Group, Shard) ->
    list_to_atom(atom_to_list(Group)++"_proxy_"++integer_to_list(Shard)).

name(Group, Shard) when is_atom(Group), is_integer(Shard) ->
    {n, l, {?MODULE, Group, Shard}}.

start_link(Group, Shard) ->
    gen_server:start_link({local, regname(Group, Shard)}, ?MODULE, [Group, Shard], []).

acquire(Group, CounterName, MaxWait) ->
    case erater_shard:shard(Group, CounterName) of
        undefined -> erater:local_acquire(Group, CounterName, MaxWait);
        Shard -> remote_acquire(Group, Shard, CounterName, MaxWait)
    end.

remote_acquire(Group, Shard, CounterName, MaxWait) ->
    case gproc:lookup_value(name(Group, Shard)) of
        undefined ->
            {error, unavailable};
        {_Manager, local} ->
            erater:local_acquire(Group, CounterName, MaxWait);
        {_Manager, Socket} when is_port(Socket) ->
            erateserver_subproto:acquire(Socket, CounterName, MaxWait);
        {_Manager, Proxy} when is_pid(Proxy) ->
            erater_proxy:acquire(Proxy, CounterName, MaxWait)
    end.


-record(proxy, {
        group,
        shard,
        manager,
        socket,
        ping_timer,
        pings_lost = 0
        }).

init([Group, Shard]) ->
    Self = self(),
    case gproc:reg_or_locate(name(Group, Shard)) of
        {Self, _} ->
            self() ! timeout, % Simulate timeout to be sure message arrives
            {ok, #proxy{group=Group, shard=Shard}};
        {Existing, _} ->
            {stop, {exists, Existing}}
    end.

handle_info(timeout, #proxy{group=Group, shard=Shard, manager=undefined} = State) ->
    case global:whereis_name(erater_shard:name(Group, Shard)) of
        undefined ->
            erlang:send_after(100, self(), timeout),
            {noreply, State};
        Manager ->
            {noreply, start_timer(connect(Manager, State))}
    end;
handle_info({'DOWN', _, process, Manager, _}, #proxy{manager = Manager} = State) ->
    self() ! timeout, % Simulate timeout to be sure message arrives
    {noreply, stop_timer(forget_connection(State))};
handle_info({_, Socket, BinResponse}, #proxy{socket = Socket} = State) ->
    erateserver_subproto:handle_message(BinResponse),
    {noreply, State};
handle_info({timeout, Timer, ping}, #proxy{ping_timer = Timer, pings_lost = Lost, socket = Socket} = State) ->
    Lost < 4 orelse exit(no_ping),
    erateserver_subproto:send_ping(Socket),
    {noreply, start_timer(State#proxy{pings_lost = Lost + 1})};
handle_info({erater_response, _, pong}, #proxy{pings_lost = Lost} = State) ->
    {noreply, State#proxy{pings_lost = max(0, Lost - 1)}};
handle_info(_, #proxy{} = State) ->
    {noreply, State}.

handle_cast(_, #proxy{} = State) ->
    {noreply, State}.

handle_call(_, _, #proxy{} = State) ->
    {reply, {error, not_implemented}, State}.

code_change(_, #proxy{} = State, _) ->
    {ok, State}.

terminate(_, _) ->
    ok.



connect(Manager, #proxy{group=Group} = State) ->
    monitor(process, Manager),
    Node = node(Manager),
    Socket = case Node == node() of
        true -> local;
        false -> open_rpc(Node, Group)
    end,
    remember_connection(State#proxy{manager = Manager, socket = Socket}).

open_rpc(Node, Group) ->
    case erateserver:conf(rpc, proxy) of
        proxy -> start_proxy(Node, Group);
        socket -> open_connection(Node, Group)
    end.

start_proxy(Node, Group) ->
    {ok, Proxy} = rpc:call(Node, erater_sup, add_proxy, [Group, self()]),
    Proxy.

open_connection(Node, Group) ->
    [_, BinHost] = binary:split(atom_to_binary(Node, latin1), <<"@">>),
    Port = erateserver:conf(port),
    [{_, UriSegment, _}] = [GroupCfg || {GroupId, _, _} = GroupCfg <- erateserver:conf(groups), GroupId == Group],
    {ok, Socket} = erateserver_subproto:client_connect(binary_to_list(BinHost), Port, UriSegment),
    Socket.

remember_connection(#proxy{group = Group, shard = Shard, manager = Manager, socket = Socket} = State) ->
    gproc:set_value(name(Group, Shard), {Manager, Socket}),
    State.

forget_connection(#proxy{group = Group, shard = Shard} = State) ->
    gproc:set_value(name(Group, Shard), undefined),
    State#proxy{manager = undefined, socket = undefined}.

start_timer(#proxy{socket = Socket} = State) when is_port(Socket) ->
    Timer = erlang:start_timer(250, self(), ping),
    State#proxy{ping_timer = Timer};
start_timer(#proxy{} = State) ->
    % We don't need timer for local or proxy connections
    State.

stop_timer(#proxy{ping_timer = Timer} = State) when is_reference(Timer) ->
    erlang:cancel_timer(Timer),
    State#proxy{ping_timer = undefined};
stop_timer(#proxy{} = State) ->
    % nothing to stop
    State.
