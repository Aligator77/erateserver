-module(erateserver_config).
-export([load/1]).

load(undefined) ->
    ok;
load(ConfigPath) ->
    [Config] = yamerl_constr:file(ConfigPath),

    CommonConfig = proplists:get_value("common", Config, []),
    ClusterSize = length(erater_pinger:nodes_to_ping()),
    SpareHosts = proplists:get_value("spare_hosts", CommonConfig, (ClusterSize div 5) + 1),
    DefaultShards = max(1, ClusterSize - SpareHosts),

    HttpConfig = proplists:get_value("http", Config, []),
    [load_http_param(list_to_atom(Key), Value) || {Key, Value} <- HttpConfig],

    GroupsConfig = proplists:get_value("groups", Config, undefined),
    load_groups_config(GroupsConfig, [{shards, DefaultShards}]).

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

read_group_opt("rps", RPS) when is_integer(RPS) ->
    {rps, RPS};
read_group_opt("burst", Burst) when is_integer(Burst) ->
    {capacity, Burst};
read_group_opt("shard_count", Shards) when is_integer(Shards) ->
    {shards, Shards};
read_group_opt("ttl", TTL) when is_integer(TTL) ->
    {ttl, TTL}.

