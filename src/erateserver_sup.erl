%% -*- mode: erlang -*-
-module(erateserver_sup).
-behavior(supervisor).

-export([start_link/0]).

-export([init/1]).


start_link() ->
    supervisor:start_link({local, erateserver}, ?MODULE, app_root).

init(app_root) ->
    {ok, {{one_for_one, 10, 5}, []}}.
