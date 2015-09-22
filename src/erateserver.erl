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
    {ok, _} = application:ensure_all_started(?MODULE, permanent),
    ok.

start(_, _) ->
    erateserver_config:load(conf(config, undefined)),
    StartResult = erateserver_sup:start_link(),
    ok = erateserver_listener:configure_groups(),
    StartResult.

stop(_) ->
    ok.

