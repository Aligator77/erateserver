%% -*- mode: erlang -*-
-module(erateserver).
-behavior(application).

-export([start/0]).
-export([conf/1, conf/2]).

-export([start/2, stop/1]).
-export([configure_groups/0]).

conf(port) ->
    conf(port, 8080);
conf(Key) ->
    {ok, Value} = application:get_env(?MODULE, Key),
    Value.
conf(Key, Default) ->
    application:get_env(?MODULE, Key, Default).

start() ->
    {ok, _} = application:ensure_all_started(?MODULE),
    ok.

start(_, _) ->
    erateserver_config:load(conf(config, undefined)),
    StartResult = erateserver_sup:start_link(),
    {ok, _} = start_server(conf(port), conf(pool_size, 100), conf(groups, []), conf_hooks()),
    StartResult.

stop(_) ->
    ok.

%% Reconfigure erater groups
configure_groups() ->
    Groups = conf(groups, []),
    ok = validate_groups(Groups),
    Dispatch = make_dispatch(Groups),
    cowboy:set_env(?MODULE, dispatch, Dispatch).


start_server(_Port, _PoolSize, [], _Hooks) ->
    ignore;
start_server(Port, PoolSize, Groups, Hooks) when is_list(Groups) ->
    ok = validate_groups(Groups),
    CowboyEnv = [{env, [{dispatch, make_dispatch(Groups)}]}],
    Opts = [{max_keepalive, 100000}, {timeout, 300000}],
    PoolOpts = [{ip, {0,0,0,0,0,0,0,0}}, {port, Port}, {max_connections, 100000}],
    cowboy:start_http(?MODULE, PoolSize, PoolOpts, CowboyEnv ++ Hooks ++ Opts).

segment_blacklist() ->
    ["ping", "rpc", "admin"].

% Group config validation
validate_groups([]) ->
    ok;
validate_groups([{Name, UrlSegment, GroupConfig}|MoreGroups]) when is_atom(Name), is_list(UrlSegment), is_list(GroupConfig) ->
    % Seems legit. Go deeper
    StrippedSeg = string:strip(UrlSegment, both, $/),
    Blacklisted = lists:member(StrippedSeg, segment_blacklist()),
    ValidConfig = (length(erater_config:clean(GroupConfig)) > 2),
    case {Blacklisted, ValidConfig} of
        {false, true} ->
            validate_groups(MoreGroups); % ok
        {true, _} ->
            lager:critical("Blacklisted uri segment in erateserver config: ~p", [UrlSegment]),
            {error, {blacklisted, UrlSegment}};
        {_, false} ->
            lager:critical("Bad config for erateserver group ~w: ~p", [Name, GroupConfig]),
            {error, {bad_config, GroupConfig}}
    end;
validate_groups([BadGroup|_]) ->
    {error, {bad_group_definition, BadGroup}}.


make_dispatch(Groups) ->
    PathList = [configure_group(Group) || Group <- Groups],
    RPCList = [configure_rpc(Group) || Group <- Groups],
    AdminList = erateserver_admin:path_list(),
    DefPath = {'_', erateserver_handler, []},
    Host = {'_', PathList ++ RPCList ++ AdminList ++ [DefPath]},
    cowboy_router:compile([Host]).

configure_group({GroupName, UrlSegment, GroupConfig}) ->
    erater:configure(GroupName, GroupConfig),
    PathMatch = "/" ++ UrlSegment ++ "/:counter_name", 
    {PathMatch, erateserver_handler, [GroupName]}.

configure_rpc({GroupName, UrlSegment, GroupConfig}) ->
    Shards = erater_config:shards(GroupConfig),
    erateserver_sup:add_proxies(GroupName, Shards),
    {"/rpc/" ++ UrlSegment, erateserver_subproto, GroupName}.

conf_hooks() ->
    case conf(log_access, false) of
        true -> [{onresponse, fun erateserver_log:access/4}];
        false -> []
    end.
