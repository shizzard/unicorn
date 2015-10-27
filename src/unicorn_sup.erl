-module(unicorn_sup).
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
        unicorn_worker_sup, {unicorn_worker_sup, start_link, []},
        permanent, brutal_kill, supervisor, [unicorn_worker_sup]
    },{
        unicorn_nofile_worker_sup, {unicorn_nofile_worker_sup, start_link, []},
        permanent, brutal_kill, supervisor, [unicorn_nofile_worker_sup]
    }],
    {ok, {{one_for_one, 1000, 3600}, ChildSpecs}}.



%% Internals


