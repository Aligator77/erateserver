%% -*- mode: erlang -*-
-module(erateserver_handler).
-behavior(cowboy_http_handler).
 
-export([init/3]).
-export([handle/2]).
-export([terminate/3]).
 
% init: track request number
init(Type, Req, Options) ->
    increment_rq_num(),
    do_init(Type, Req, Options).

do_init(_Type, Req, []) ->
    {ok, Req, wrong_path};
do_init(_Type, Req, [GroupName]) ->
    {ok, Req, {erater, GroupName}}.
 
handle(Req0, {erater, GroupName} = State) ->
    {CounterName, Req} = cowboy_req:binding(counter_name, Req0),
    {ok, Req2} = case erateserver_shard_proxy:acquire(GroupName, CounterName, 0) of
        {ok, _} ->
            cowboy_req:reply(200, [], [], Req);
        {error, overflow} ->
            cowboy_req:reply(429, [], [], Req);
        {error, unavailable} ->
            cowboy_req:reply(503, [], [], Req)
    end,
    {ok, Req2, State};

handle(Req, wrong_path = State) ->
    {ok, Req2} = cowboy_req:reply(404, [{<<"content-type">>, <<"text/plain">>}], <<"Wrong URI path!\n">>, Req),
    {ok, Req2, State}.
 
terminate(_Reason, _Req, _State) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internals
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% We store request number in process dictionary because we have nothing else persistent
increment_rq_num() ->
    PrevRqNum = case get(rq_num) of
        undefined -> 0;
        ExistingNum -> ExistingNum
    end,
    put(rq_num, PrevRqNum + 1).
