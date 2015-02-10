%% -*- mode: erlang -*-
-module(erateserver_admin).
-behavior(cowboy_http_handler).

-export([path_list/0]).
-export([init/3, handle/2, terminate/3]).

path_list() ->
    PingPath = {"/ping", ?MODULE, ping},
    ReloadPath = {"/admin/reload_config", ?MODULE, reload_config},
    [PingPath, ReloadPath].


init(_Type, Req, Opt) ->
    {ok, erateserver_log:start_request(Req), Opt}.

handle(Req, ping) ->
    {ok, Req2} = cowboy_req:reply(200, [{<<"content-type">>, <<"text/plain">>}], <<"pong\n">>, Req),
    {ok, Req2, ping};

handle(Req, reload_config) ->
    % Reload server config file into application environment
    _ = erateserver_config:load(erateserver:conf(config)),
    % Apply fresh groups config
    ok = erateserver:configure_groups(),

    {ok, Req2} = cowboy_req:reply(200, [{<<"content-type">>, <<"text/plain">>}], <<"reconfigured\n">>, Req),
    {ok, Req2, reload_config}.


terminate(_Reason, _Req, _State) ->
    ok.
