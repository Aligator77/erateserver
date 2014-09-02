%% -*- mode: erlang -*-
-module(erateserver_health).
-behavior(cowboy_http_handler).
 
-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Type, Req, ping) ->
    erateserver_handler:increment_rq_num(),
    {ok, Req, ping}.

handle(Req, ping) ->
    {ok, Req2} = cowboy_req:reply(200, [{<<"content-type">>, <<"text/plain">>}], <<"pong\n">>, Req),
    {ok, Req2, ping}.

terminate(_Reason, _Req, _State) ->
    ok.
