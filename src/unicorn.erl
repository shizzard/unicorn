-module(unicorn).

-include("unicorn.hrl").

-export([dev_start/0]).

-export([use/1, subscribe/2, unsubscribe/1, unsubscribe/2, reload/1]).



%% Interface



use(File) when is_binary(File) ->
    case supervisor:start_child(unicorn_sup, [File]) of
        {ok, _Pid} ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.



subscribe(File, Path) when is_binary(File), is_list(Path) ->
    ProcName = ?FILE_TO_NAME(File),
    gen_server:call(ProcName, ?SUBSCRIBE(self(), Path)).



unsubscribe(File) when is_binary(File) ->
    ProcName = ?FILE_TO_NAME(File),
    gen_server:call(ProcName, ?UNSUBSCRIBE(self())).

unsubscribe(File, Path) when is_binary(File), is_list(Path) ->
    ProcName = ?FILE_TO_NAME(File),
    gen_server:call(ProcName, ?UNSUBSCRIBE(self(), Path)).



reload(File) when is_binary(File) ->
    ProcName = ?FILE_TO_NAME(File),
    gen_server:call(ProcName, ?RELOAD).



dev_start() ->
    application:start(etoml),
    application:start(unicorn).

