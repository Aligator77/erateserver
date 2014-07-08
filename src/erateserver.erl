%% -*- mode: erlang -*-
-module(erateserver).
-behavior(application).

-export([start/0]).
-export([conf/1, conf/2]).

-export([start/2, stop/1]).

conf(Key) ->
    {ok, Value} = application:get_env(?MODULE, Key),
    Value.
conf(Key, Default) ->
    application:get_env(?MODULE, Key, Default).

start() ->
    {ok, _} = application:ensure_all_started(?MODULE),
    ok.

start(_, _) ->
    StartResult = erateserver_sup:start_link(),
    {ok, _} = start_server(conf(port, 8080), conf(pool_size, 100), conf(groups, []), conf_hooks()),
    StartResult.

stop(_) ->
    ok.

start_server(_Port, _PoolSize, [], _Hooks) ->
    ignore;
start_server(Port, PoolSize, Groups, Hooks) when is_list(Groups) ->
    PathList = [configure_group(Group) || Group <- Groups],
    DefPath = {'_', erateserver_handler, []},
    Host = {'_', PathList ++ [DefPath]},
    Dispatch = cowboy_router:compile([Host]),
    Opts = [{max_keepalive, 100000}, {timeout, 300000}],
    PoolOpts = [{port, Port}, {max_connections, 100000}],
    cowboy:start_http(?MODULE, PoolSize, PoolOpts, [{env, [{dispatch, Dispatch}]}] ++ Hooks ++ Opts).

configure_group({GroupName, UrlSegment, GroupConfig}) ->
    erater:configure(GroupName, GroupConfig),
    PathMatch = "/" ++ UrlSegment ++ "/:counter_name", 
    {PathMatch, erateserver_handler, [GroupName]}.

conf_hooks() ->
    case conf(log_access, false) of
        true -> [{onresponse, fun erateserver_log:access/4}];
        false -> []
    end.
