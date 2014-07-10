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
        {_Manager, Socket} ->
            erateserver_subproto:acquire(Socket, CounterName, MaxWait)
    end.


-record(proxy, {
        group,
        shard,
        manager,
        socket
        }).

init([Group, Shard]) ->
    Self = self(),
    case gproc:reg_or_locate(name(Group, Shard)) of
        {Self, _} ->
            {ok, #proxy{group=Group, shard=Shard}, 0};
        {Existing, _} ->
            {stop, {exists, Existing}}
    end.

handle_info(timeout, #proxy{group=Group, shard=Shard, manager=undefined} = State) ->
    case global:whereis_name(erater_shard:name(Group, Shard)) of
        undefined ->
            {noreply, State, 100};
        Manager ->
            {noreply, connect(Manager, State)}
    end;
handle_info({'DOWN', _, process, Manager, _}, #proxy{manager = Manager} = State) ->
    {noreply, forget_connection(State), 0};
handle_info({_, Socket, BinResponse}, #proxy{socket = Socket} = State) ->
    erateserver_subproto:handle_message(BinResponse),
    {noreply, State};
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
    [_, BinHost] = binary:split(atom_to_binary(Node, latin1), <<"@">>),
    Port = erateserver:conf(port),
    [{_, UriSegment, _}] = [GroupCfg || {GroupId, _, _} = GroupCfg <- erateserver:conf(groups), GroupId == Group],
    {ok, Socket} = erateserver_subproto:client_connect(binary_to_list(BinHost), Port, UriSegment),
    remember_connection(State#proxy{manager = Manager, socket = Socket}).

remember_connection(#proxy{group = Group, shard = Shard, manager = Manager, socket = Socket} = State) ->
    gproc:set_value(name(Group, Shard), {Manager, Socket}),
    State.

forget_connection(#proxy{group = Group, shard = Shard} = State) ->
    gproc:set_value(name(Group, Shard), undefined),
    State#proxy{manager = undefined, socket = undefined}.
