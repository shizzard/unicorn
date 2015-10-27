-module(unicorn_nofile_worker_sup).
-include("supervisor.hrl").

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).



%% Interface



%%



%% Callbacks



start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    ChildSpecs = [{
        unicorn_nofile_worker, {unicorn_nofile_worker, start_link, []},
        transient, brutal_kill, worker, [unicorn_nofile_worker]
    }],
    {ok, {{simple_one_for_one, 1000, 3600}, ChildSpecs}}.



%% Internals


