-module(erateserver_log).
-compile({parse_transform, lager_transform}).

-export([access/4]).
-export([start_request/1]).

% init: track request number
start_request(Req) ->
    ReqStarted = cowboy_req:set_meta(start_time, os:timestamp(), Req),
    increment_rq_num(),
    ReqStarted.

access(Status, Headers, Body, Req) ->
    {[Peer, Method, Url], Req2} = lists:mapfoldl(fun get_req_prop/2, Req, [peer, method, url]),
    ReqTime = get_req_time(Req2),
    {ok, ReqReplied} = cowboy_req:reply(Status, Headers, Body, Req2),
    lager:info([
            {tag, access},
            {rq_num, get(rq_num)},
            {rq_time, io_lib:format("~.3f", [ReqTime])},
            {peer, peer_string(Peer)},
            {method, Method},
            {url, Url},
            {status, Status}
            ], ""),
    ReqReplied.

get_req_prop(Prop, Req) ->
    cowboy_req:Prop(Req).

%% Fetch request start time and return non-negative diff in seconds (float)
get_req_time(Req) ->
    FinishTime = os:timestamp(),
    {StartTime, _} = cowboy_req:meta(start_time, Req, FinishTime),
    UsDiff = timer:now_diff(FinishTime, StartTime),
    if
        UsDiff < 0 -> 0.0; % Protect against system clock going backwards
        true -> UsDiff/1000000
    end.

peer_string({PeerAddr, _Port}) ->
    inet_parse:ntoa(PeerAddr);
peer_string(_) ->
    "-".

%% We store request number in process dictionary because we have nothing else persistent
%%    Well, Cowboy's internal state (cowboy_protocol) has needed value (#state.req_keepalive),
%%    but it is not available to user code
increment_rq_num() ->
    PrevRqNum = case get(rq_num) of
        undefined -> 0;
        ExistingNum -> ExistingNum
    end,
    put(rq_num, PrevRqNum + 1).
