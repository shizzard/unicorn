-module(unicorn).

-include("unicorn.hrl").

-export([dev_start/0]).

-export([load/2, unload/1, subscribe/2, unsubscribe/1, unsubscribe/2, reload/1, get/2, to_document/1]).

-type document() :: document_object() | document_list() | document_scalar().
-type document_object() :: {list({Key :: document_scalar(), Value :: document()})}.
-type document_list() :: list(Item :: document()).
-type document_scalar() :: binary() | integer() | float() | atom().
-type loader() :: fun((filename(), binary()) -> document()).
-type filename() :: binary().
-type path() :: list(document_scalar()).
-type error() :: {error, Reason :: any()}.

-export_type([document/0, document_object/0, document_list/0, document_scalar/0]).
-export_type([loader/0, filename/0, path/0, error/0]).



%% Interface



-spec load(File :: filename(), Loader :: loader()) ->
    ok | error().
load(File, Loader) when is_binary(File), is_function(Loader, 2)  ->
    ProcName = ?FILE_TO_NAME(File),
    case whereis(ProcName) of
        undefined ->
            case supervisor:start_child(unicorn_sup, [File, Loader]) of
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
    {ok, Value :: any()} | {error, undefined}.
get(_Path, undefined) ->
    {error, undefined};

get(Key, Value) when not is_list(Key) ->
    get([Key], Value);

get([], Value) ->
    {ok, Value};

get([Key | Path], {Proplist}) ->
    Struct = proplists:get_value(Key, Proplist),
    get(Path, Struct);

get(Path, _Value) ->
    get(Path, undefined).



-spec to_document(Item :: any()) ->
    Document :: document().
to_document(Item) ->
    IsProplist = is_proplist(Item),
    if
        IsProplist ->
            to_document_pl(Item);
        is_list(Item) ->
            to_document_l(Item);
        true ->
            Item
    end.



-spec to_document_pl(Proplist :: list({Key :: document_scalar(), Value :: any()})) ->
    Document :: document_object().
to_document_pl(Proplist) ->
    lists:foldl(fun({Key, Value}, {Acc}) ->
        {Acc ++ [{Key, to_document(Value)}]}
    end, {[]}, Proplist).



-spec to_document_l(List :: list()) ->
    Document :: document_list().
to_document_l(List) ->
    lists:foldl(fun(Value, Acc) ->
        Acc ++ [to_document(Value)]
    end, [], List).



-spec dev_start() ->
    ok.
dev_start() ->
    application:start(unicorn).



%% Internals



-spec is_proplist(Item :: any()) ->
    Result :: true | false.
is_proplist(Item) when is_list(Item) ->
    lists:all(fun
        ({_, _}) -> true;
        (_) -> false
    end, Item);

is_proplist(_Item) ->
    false.