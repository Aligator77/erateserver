%% -*- mode: erlang -*-
-module(erateserver_sup).
-behavior(supervisor).

-export([start_link/0]).
-export([add_proxies/2, start_link_proxies/2]).

-export([init/1]).

proxies_name(Group) ->
    list_to_atom(atom_to_list(Group) ++ "_proxies").

start_link() ->
    supervisor:start_link({local, erateserver}, ?MODULE, app_root).

start_link_proxies(Group, Shards) ->
    supervisor:start_link({local, proxies_name(Group)}, ?MODULE, {proxies, Group, Shards}).


add_proxies(Group, Shards) ->
    supervisor:start_child(erateserver, proxy_sup_spec(Group, Shards)).

proxy_sup_spec(Group, Shards) ->
    {{proxies, Group},
     {?MODULE, start_link_proxies, [Group, Shards]},
     permanent, 1000, supervisor, []}.

proxy_spec(Group, Shard) ->
    {{shard_proxy, Shard},
     {erateserver_shard_proxy, start_link, [Group, Shard]},
     permanent, 1000, worker, [erateserver_shard_proxy]}.

init(app_root) ->
    {ok, {{one_for_one, 10, 5}, []}};
init({proxies, Group, Shards}) ->
    ProxySpecs = [proxy_spec(Group, Shard) || Shard <- lists:seq(1, Shards)],
    {ok, {{one_for_one, 20, 1}, ProxySpecs}}.
