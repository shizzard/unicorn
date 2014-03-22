-module(unicorn).

-include("unicorn.hrl").

-export([dev_start/0]).

-export([load/1, unload/1, subscribe/2, unsubscribe/1, unsubscribe/2, reload/1, get/2]).



%% Interface



load(File) when is_binary(File) ->
    ProcName = ?FILE_TO_NAME(File),
    case whereis(ProcName) of
        undefined ->
            case supervisor:start_child(unicorn_sup, [File]) of
                {ok, _Pid} ->
                    ok;
                {error, Reason} ->
                    {error, Reason}
            end;
        _Pid ->
            ok
    end.



unload(File) when is_binary(File) ->
    ProcName = ?FILE_TO_NAME(File),
    gen_server:cast(ProcName, ?TERMINATE).



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



get([], Document) ->
    Document;

get([Key | Keys], Document) when is_list(Document) ->
    case proplists:get_value(Key, Document) of
        undefined ->
            undefined;
        Value ->
            get(Keys, Value)
    end;

get([_Key | _Keys], _Document) ->
    undefined.



dev_start() ->
    application:start(etoml),
    application:start(unicorn).

