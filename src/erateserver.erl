%% -*- mode: erlang -*-
-module(erateserver).
-behavior(application).

-export([start/0]).
-export([conf/1, conf/2]).

-export([start/2, stop/1]).

conf(port) ->
    conf(port, 8080);
conf(Key) ->
    {ok, Value} = application:get_env(?MODULE, Key),
    Value.
conf(Key, Default) ->
    application:get_env(?MODULE, Key, Default).

start() ->
    {ok, _} = application:ensure_all_started(?MODULE),
    ok.

start(_, _) ->
    erateserver_config:load(conf(config)),
    StartResult = erateserver_sup:start_link(),
    {ok, _} = start_server(conf(port), conf(pool_size, 100), conf(groups, []), conf_hooks()),
    StartResult.

stop(_) ->
    ok.

start_server(_Port, _PoolSize, [], _Hooks) ->
    ignore;
start_server(Port, PoolSize, Groups, Hooks) when is_list(Groups) ->
    CowboyEnv = [{env, [{dispatch, make_dispatch(Groups)}]}],
    Opts = [{max_keepalive, 100000}, {timeout, 300000}],
    PoolOpts = [{ip, {0,0,0,0,0,0,0,0}}, {port, Port}, {max_connections, 100000}],
    cowboy:start_http(?MODULE, PoolSize, PoolOpts, CowboyEnv ++ Hooks ++ Opts).

make_dispatch(Groups) ->
    PathList = [configure_group(Group) || Group <- Groups],
    RPCList = [configure_rpc(Group) || Group <- Groups],
    PingPath = {"/ping", erateserver_health, ping},
    DefPath = {'_', erateserver_handler, []},
    Host = {'_', PathList ++ RPCList ++ [PingPath, DefPath]},
    cowboy_router:compile([Host]).

configure_group({GroupName, UrlSegment, GroupConfig}) ->
    erater:configure(GroupName, GroupConfig),
    PathMatch = "/" ++ UrlSegment ++ "/:counter_name", 
    {PathMatch, erateserver_handler, [GroupName]}.

configure_rpc({GroupName, UrlSegment, GroupConfig}) ->
    Shards = erater_config:shards(GroupConfig),
    erateserver_sup:add_proxies(GroupName, Shards),
    {"/rpc/" ++ UrlSegment, erateserver_subproto, GroupName}.

conf_hooks() ->
    case conf(log_access, false) of
        true -> [{onresponse, fun erateserver_log:access/4}];
        false -> []
    end.
