%% -*- mode: erlang -*-
-module(erateserver_handler).
-behavior(cowboy_http_handler).
 
-export([init/3]).
-export([handle/2]).
-export([terminate/3]).
 
init(_Type, Req, []) ->
    {ok, Req, wrong_path};
init(_Type, Req, [GroupName]) ->
    {ok, Req, {erater, GroupName}}.
 
handle(Req0, {erater, GroupName} = State) ->
    {CounterName, Req} = cowboy_req:binding(counter_name, Req0),
    {ok, Req2} = case erater:acquire(GroupName, CounterName, 0) of
        {ok, _} ->
            cowboy_req:reply(200, [], [], Req);
        {error, overflow} ->
            cowboy_req:reply(429, [], [], Req)
    end,
    {ok, Req2, State};

handle(Req, wrong_path = State) ->
    {ok, Req2} = cowboy_req:reply(404, [{<<"content-type">>, <<"text/plain">>}], <<"Wrong URI path!\n">>, Req),
    {ok, Req2, State}.
 
terminate(_Reason, _Req, _State) ->
    ok.
