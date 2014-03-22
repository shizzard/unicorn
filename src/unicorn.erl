-module(unicorn).

-include("unicorn.hrl").

-export([dev_start/0]).

-export([load/1, unload/1, subscribe/2, unsubscribe/1, unsubscribe/2, reload/1, get/2]).

-type error() :: {error, Reason :: any()}.
-type document() :: list({Key :: any(), Value :: document() | list() | any()}).
-type filename() :: binary().
-type path() :: list(binary()).

-export_type([error/0, document/0, filename/0, path/0]).



%% Interface



-spec load(File :: filename()) ->
    ok | error().
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



-spec unload(File :: filename()) ->
    ok.
unload(File) when is_binary(File) ->
    ProcName = ?FILE_TO_NAME(File),
    gen_server:cast(ProcName, ?TERMINATE).



-spec subscribe(File :: filename(), Path :: list(binary())) ->
    {ok, Config :: document()} | error().
subscribe(File, Path) when is_binary(File), is_list(Path) ->
    ProcName = ?FILE_TO_NAME(File),
    gen_server:call(ProcName, ?SUBSCRIBE(self(), Path)).



-spec unsubscribe(File :: filename()) ->
    ok.
unsubscribe(File) when is_binary(File) ->
    ProcName = ?FILE_TO_NAME(File),
    gen_server:call(ProcName, ?UNSUBSCRIBE(self())).

-spec unsubscribe(File :: binary(), Path :: path()) ->
    ok.
unsubscribe(File, Path) when is_binary(File), is_list(Path) ->
    ProcName = ?FILE_TO_NAME(File),
    gen_server:call(ProcName, ?UNSUBSCRIBE(self(), Path)).



-spec reload(File :: filename()) ->
    {ok, NumNotified :: integer()}.
reload(File) when is_binary(File) ->
    ProcName = ?FILE_TO_NAME(File),
    gen_server:call(ProcName, ?RELOAD).



-spec get(Path :: path(), Document :: document()) ->
    Value :: undefined | any().
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



-spec dev_start() ->
    ok.
dev_start() ->
    application:start(etoml),
    application:start(unicorn).

