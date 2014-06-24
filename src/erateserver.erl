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
    {ok, _} = start_server(conf(port, 8080), conf(pool_size, 100), conf(groups, [])),
    StartResult.

stop(_) ->
    ok.

start_server(_Port, _PoolSize, []) ->
    ignore;
start_server(Port, PoolSize, Groups) when is_list(Groups) ->
    PathList = [configure_group(Group) || Group <- Groups],
    DefPath = {'_', erateserver_handler, []},
    Host = {'_', PathList ++ [DefPath]},
    Dispatch = cowboy_router:compile([Host]),
    cowboy:start_http(?MODULE, PoolSize, [{port, Port}], [{env, [{dispatch, Dispatch}]}]).

configure_group({GroupName, UrlSegment, GroupConfig}) ->
    erater:configure(GroupName, GroupConfig),
    PathMatch = "/" ++ UrlSegment ++ "/:counter_name", 
    {PathMatch, erateserver_handler, [GroupName]}.
