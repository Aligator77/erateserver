-module(erateserver_config).
-export([load/1]).

load(undefined) ->
    ok;
load(ConfigPath) ->
    [Config] = yamerl_constr:file(ConfigPath),

    CommonConfig = proplists:get_value("common", Config, []),
    Defaults = load_common_config(CommonConfig),

    HttpConfig = proplists:get_value("http", Config, []),
    [load_http_param(list_to_atom(Key), Value) || {Key, Value} <- HttpConfig],

    GroupsConfig = proplists:get_value("groups", Config, undefined),
    load_groups_config(GroupsConfig, Defaults).

load_common_config(CommonConfig) ->
    % Set cookie specified in config (if any)
    set_cookie(node(), proplists:get_value("cookie", CommonConfig)),

    % Decode "cluster" configuration value to node list
    ClusterCfg = proplists:get_value("cluster", CommonConfig),
    Nodes = make_node_list(ClusterCfg),

    % Derive default number of shards
    ClusterSize = length(Nodes),
    SpareHosts = proplists:get_value("spare_hosts", CommonConfig, (ClusterSize div 5) + 1),
    DefaultShards = max(1, ClusterSize - SpareHosts),

    [{nodes, Nodes}, {shards, DefaultShards}].


set_cookie('nonode@nohost', _) -> ok;   % No network
set_cookie(_, undefined) -> ok;         % No cookie
set_cookie(Node, CookieStr) -> erlang:set_cookie(Node, list_to_atom(CookieStr)).

%% Interpret the "cluster" key -- get a list of nodes
make_node_list(undefined) ->
    [node()];
make_node_list([[X|_]|_] = NodeStrList) when is_integer(X) ->
    [make_node(NodeStr) || NodeStr <- NodeStrList];
make_node_list([{DiscoveryModStr, DiscoveryArg}]) -> % Custom discovery module
    DiscoveryMod = list_to_atom(DiscoveryModStr),
    [_|_] = DiscoveryMod:cluster(DiscoveryArg).

make_node("@"++_ = AtHostStr) ->
    make_remote_node(AtHostStr);
make_node(NodeStr) ->
    case lists:last(NodeStr) of
        $@ -> make_local_node(NodeStr);
        _ -> list_to_atom(NodeStr)
    end.

%% For nodes specified as "name@" we append a host part from our current node name
make_local_node(NodeNameAt) ->
    MyNodeStr = atom_to_list(node()),
    % Replace everything before and including '@' in our node name with "name@" provided in config
    NodeStr = re:replace(MyNodeStr, "^.*@", NodeNameAt, [{return, list}]),
    list_to_atom(NodeStr).

%% For nodes specified as "@host" we prepend a name part from our current node name
make_remote_node(AtHostStr) ->
    MyNodeStr = atom_to_list(node()),
    % Replace everything after and including '@' in our node name with "@host" provided in config
    NodeStr = re:replace(MyNodeStr, "@.*$", AtHostStr, [{return, list}]),
    list_to_atom(NodeStr).

load_http_param(Key, Value) ->
    application:set_env(erateserver, Key, Value).

load_groups_config(undefined, _) ->
    ok;
load_groups_config(Groups, Defaults) when is_list(Groups) ->
    AppConfig = [transform_group_config(GroupName, GroupOpts, Defaults) || {GroupName, GroupOpts} <- Groups],
    application:set_env(erateserver, groups, AppConfig).

transform_group_config(GroupName, GroupOpts, Defaults) ->
    {list_to_atom(GroupName), GroupName, read_group_opts(GroupOpts, Defaults)}.

read_group_opts([], Opts) ->
    Opts;
read_group_opts([{CKey, CValue}|Tail], Acc) ->
    {Key, Value} = read_group_opt(CKey, CValue),
    read_group_opts(Tail, lists:keystore(Key, 1, Acc, {Key, Value})).

read_group_opt("rps", RPS) when is_number(RPS) ->
    {rps, RPS};
read_group_opt("burst", Burst) when is_integer(Burst) ->
    {burst, Burst};
read_group_opt("default_wait", Wait) when is_integer(Wait) ->
    {default_wait, Wait};
read_group_opt("cluster", ClusterCfg) ->
    {nodes, make_node_list(ClusterCfg)};
read_group_opt("shard_count", Shards) when is_integer(Shards) ->
    {shards, Shards};
read_group_opt("ttl", TTL) when is_integer(TTL) ->
    {ttl, TTL}.

