%% -*- mode: erlang -*-
-module(erateserver_handler).
-behavior(cowboy_http_handler).
 
-export([init/3]).
-export([handle/2]).
-export([terminate/3]).
 
init(Type, Req, Options) ->
    ReqStarted = erateserver_log:start_request(Req),
    do_init(Type, ReqStarted, Options).

do_init(_Type, Req, []) ->
    {ok, Req, wrong_path};
do_init(_Type, Req, {Mode, GroupName}) ->
    {ok, Req, {erater, Mode, GroupName}}.
 
handle(Req0, {erater, Mode, GroupName} = State) ->
    {CounterName, Req} = cowboy_req:binding(counter_name, Req0),
    {MaxWait, ClientWaits, Req1} = get_wait_args(Req, GroupName),
    {AcqOptions, ReqParsed} = get_acq_options(Req1, Mode),
    {ok, ReqFinal} = case erateserver_shard_proxy:acquire(GroupName, CounterName, MaxWait, AcqOptions) of
        {ok, Wait} ->
            wait_reply(Wait, ClientWaits, ReqParsed);
        {error, overflow} ->
            cowboy_req:reply(429, [{<<"Retry-After">>, <<"1">>}], <<>>, ReqParsed);
        {error, unavailable} ->
            cowboy_req:reply(503, [], <<>>, ReqParsed)
    end,
    {ok, ReqFinal, State};

handle(Req, wrong_path = State) ->
    {ok, Req2} = cowboy_req:reply(404, [{<<"content-type">>, <<"text/plain">>}], <<"Wrong URI path!\n">>, Req),
    {ok, Req2, State}.
 
terminate(_Reason, _Req, _State) ->
    ok.


%% Helper: parse QS to get wait info
get_wait_args(Req0, GroupName) ->
    {MaxWaitVal, Req1} = cowboy_req:qs_val(<<"max_wait_ms">>, Req0),
    {ClientWaitsVal, Req2} = cowboy_req:qs_val(<<"client_waits">>, Req1),
    {parse_maxwait(MaxWaitVal, GroupName), parse_clientwaits(ClientWaitsVal), Req2}.

parse_maxwait(MaxWaitVal, _GroupName) when is_binary(MaxWaitVal) ->
    binary_to_integer(MaxWaitVal);
parse_maxwait(_undefined, GroupName) ->
    erater_group:get_config(GroupName, default_wait).

parse_clientwaits(<<"true">>) -> true;
parse_clientwaits(<<"false">>) -> false;
parse_clientwaits(true) -> true;
parse_clientwaits(undefined) -> false.


%% Parse ad-hoc options when needed
get_acq_options(Req, adhoc) ->
    lists:foldl(fun get_adhoc_option/2, {[], Req}, [rps, burst, ttl]);
get_acq_options(Req, _) ->
    {[], Req}.

get_adhoc_option(Key, {KVs, Req}) ->
    BinKey = atom_to_binary(Key, latin1),
    {BinValue, Req1} =  cowboy_req:qs_val(BinKey, Req, undefined),
    AddKVs = case {Key, BinValue} of
        {_, BadValue} when BadValue == undefined; BadValue == true ->
            [];
        {rps, BinRPS} ->
            Value = binary_to_number(BinRPS),
            [{rps, Value}];
        {_, BinValue} ->
            Value = binary_to_integer(BinValue),
            [{Key, Value}]
    end,
    {AddKVs ++ KVs, Req1}.

%% Helper: parse integer or float
binary_to_number(Bin) ->
    case binary:match(Bin, <<".">>) of
        nomatch -> binary_to_integer(Bin);
        {_, _} -> binary_to_float(Bin)
    end.


%% Waiting magic
wait_reply(0, _, Req) ->
    % Nothing to wait for, just return 200 now
    cowboy_req:reply(200, [], <<>>, Req);
wait_reply(Wait, false, Req) ->
    % Client cannot wait, so we wait for him
    timer:sleep(Wait),
    cowboy_req:reply(200, [], <<>>, Req);
wait_reply(Wait, true, Req) ->
    % Client handles waiting itself, so pass waiting time in body KV
    Headers = [{<<"content-type">>, <<"application/x-www-form-urlencoded">>}],
    cowboy_req:reply(202, Headers, io_lib:format("wait_ms=~w~n", [Wait]), Req).
