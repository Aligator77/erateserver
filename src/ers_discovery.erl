%%% Demo discovery module.
%%% Instead of some real-life discovery, it just generates all possible nodes
%%% from given lists of names and hosts
%%%
%%% Configuration using this module looks like this:
%%%     cluster: {ers_discovery: {names: [ers1, ers2], hosts: [__myhost__, otherhost]}}
%%% This leads to a call
%%%     ers_discovery:cluster([{"names",["ers1","ers2"]},{"hosts",["__myhost__","otherhost"]}])
%%%     (Note that options are passed as-is after parsing YAML
-module(ers_discovery).

-export([cluster/1]).


cluster(Options) ->
    Names = proplists:get_value("names", Options, []),
    Hosts = proplists:get_value("hosts", Options, []),
    [list_to_atom(Name ++ "@" ++ make_host(Host)) || Host <- Hosts, Name <- Names].

make_host("__myhost__") ->
    % Copy a host from current node name
    tl(lists:dropwhile(fun(X) -> X /= $@ end, atom_to_list(node())));
make_host(Host) ->
    Host.
