%% -*- mode: erlang -*-
-module(erateserver_admin).
-behavior(cowboy_http_handler).

-export([path_list/0]).
-export([init/3, handle/2, terminate/3]).

path_list() ->
    PingPath = {"/ping", ?MODULE, ping},
    ReloadPath = {"/admin/reload_config", ?MODULE, reload_config},
    StatusPaths = [
            {"/admin/status", ?MODULE, status},
            {"/admin/status/:group", ?MODULE, group_status} ],
    [PingPath, ReloadPath] ++ StatusPaths.


init(_Type, Req, Opt) ->
    {ok, erateserver_log:start_request(Req), Opt}.

handle(Req, ping) ->
    {ok, Req2} = cowboy_req:reply(200, [{<<"content-type">>, <<"text/plain">>}], <<"pong\n">>, Req),
    {ok, Req2, ping};

handle(Req, status) ->
    Groups = erater:groups(),
    Report = [[short_group_status(Group), "\n"] || Group <- Groups],
    {ok, Req2} = cowboy_req:reply(200, [{<<"content-type">>, <<"text/plain">>}], Report, Req),
    {ok, Req2, status};

handle(Req0, group_status) ->
    {GroupBin, Req} = cowboy_req:binding(group, Req0),
    Group = binary_to_existing_atom(GroupBin, utf8),
    true = lists:member(Group, erater:groups()),
    Report = full_group_status(Group),
    {ok, Req2} = cowboy_req:reply(200, [{<<"content-type">>, <<"text/plain">>}], Report, Req),
    {ok, Req2, status};

handle(Req, reload_config) ->
    % Reload server config file into application environment
    _ = erateserver_config:load(erateserver:conf(config)),
    % Apply fresh groups config
    ok = erateserver_listener:configure_groups(),

    {ok, Req2} = cowboy_req:reply(200, [{<<"content-type">>, <<"text/plain">>}], <<"reconfigured\n">>, Req),
    {ok, Req2, reload_config}.


terminate(_Reason, _Req, _State) ->
    ok.



short_group_status(Group) ->
    try
        {Status, Summary, _Nodes} = erater_group:status(Group),
        #{nodes := {NT, NA}, shards := {ST, SA}} = Summary,
        io_lib:format("~w (~w): ~w of ~w shards on ~w of ~w nodes", [Group, Status, SA, ST, NA, NT])
    catch
        Class:Reason ->
            io_lib:format("~w **** EXCEPTION ~w:~99999p", [Group, Class, Reason])
    end.

full_group_status(Group) ->
    try
        {Status, Summary, NodeStatuses} = erater_group:status(Group),
        #{nodes := {NT, NA}, shards := {ST, SA}} = Summary,
        Head = io_lib:format("~w (~w): ~w of ~w shards on ~w of ~w nodes", [Group, Status, SA, ST, NA, NT]),
        Body = [io_lib:format("  - ~w:  ~120p~n", [Node, NStatus]) || {Node, NStatus} <- maps:to_list(NodeStatuses)],
        [Head, "\n", Body]
    catch
        Class:Reason ->
            Stacktrace = erlang:get_stacktrace(),
            io_lib:format("~w **** EXCEPTION ~w:~99999p~n~120p", [Group, Class, Reason, Stacktrace])
    end.
