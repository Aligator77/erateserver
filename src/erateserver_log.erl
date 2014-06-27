-module(erateserver_log).
-compile({parse_transform, lager_transform}).

-export([access/4]).

access(Status, Headers, Body, Req) ->
    {[{PeerAddr, _Port}, Method, Url], Req2} = lists:mapfoldl(fun get_req_prop/2, Req, [peer, method, url]),
    {ok, ReqReplied} = cowboy_req:reply(Status, Headers, Body, Req2),
    lager:info([{tag, access}, {rq_num, get(rq_num)}, {peer, inet_parse:ntoa(PeerAddr)}, {method, Method}, {url, Url}, {status, Status}], ""),
    ReqReplied.

get_req_prop(Prop, Req) ->
    cowboy_req:Prop(Req).
