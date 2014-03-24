-module(unicorn).

-include("unicorn.hrl").

-export([load/3, unload/1, subscribe/2, unsubscribe/1, unsubscribe/2, reload/1, get/2, to_document/1]).

-ifdef(UNICORN_DEVEL).
    -export([dev_start/0, dev_loader/1, dev_validator/1]).
-endif.

-type document() :: document_object() | document_list() | document_scalar().
-type document_object() :: {list({Key :: document_scalar(), Value :: document()})}.
-type document_list() :: list(Item :: document()).
-type document_scalar() :: binary() | integer() | float() | atom().
-type loader() :: fun((binary()) -> {ok, document()} | error()).
-type validator() :: fun((document()) -> {ok, document()} | error()).
-type filename() :: binary().
-type path() :: list(document_scalar()).
-type error() :: {error, Reason :: any()}.

-export_type([document/0, document_object/0, document_list/0, document_scalar/0]).
-export_type([loader/0, validator/0, filename/0, path/0, error/0]).



%% Interface



-spec load(File :: filename(), Loader :: loader(), Validator :: validator()) ->
    ok | error().
load(File, Loader, Validator) when is_binary(File), is_function(Loader, 1), is_function(Validator, 1)  ->
    ProcName = ?FILE_TO_NAME(File),
    case whereis(ProcName) of
        undefined ->
            case supervisor:start_child(unicorn_sup, [File, Loader, Validator]) of
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



%% Dev functions



-ifdef(UNICORN_DEVEL).



    -spec dev_start() ->
        ok.
    dev_start() ->
        application:start(etoml),
        application:start(unicorn).



    -spec dev_loader(RawDocument :: binary()) ->
        {ok, Document :: document()} | error().
    dev_loader(RawDocument) ->
        case etoml:parse(RawDocument) of
            {ok, Document} ->
                {ok, to_document(Document)};
            {error, Error} ->
                {error, Error}
        end.



    -spec dev_validator(Document :: document()) ->
        {ok, Document :: document()} | error().
    dev_validator(Document0) ->
        case jiffy_v:validate(jiffy_v_map(), Document0, fun jiffy_validator/3) of
            {[], Document} ->
                {ok, Document};
            {Errors, _Result} ->
                {error, Errors}
        end.



    -spec jiffy_v_map() ->
        tuple().
    jiffy_v_map() ->
        {hash, [
            {<<"version">>, required, {string}},
            {<<"database">>, required, {hash, [
                {<<"server">>, required, {string}},
                {<<"port">>, required, {integer}},
                {<<"pool_size">>, required, {integer}},
                {<<"max_overflow">>, required, {integer}}
            ]}},
            {<<"binding">>, required, {hash, [
                {<<"proto1">>, required, {hash, [
                    {<<"ports">>, required, {list, [{integer}]}},
                    {<<"conn_timeout">>, required, {integer}}
                ]}},
                {<<"proto2">>, required, {hash, [
                    {<<"ports">>, required, {list, [{integer}]}},
                    {<<"conn_timeout">>, required, {integer}}
                ]}}
            ]}},
            {<<"worker">>, required, {hash, [
                {<<"proto1">>, required, {hash, [
                    {<<"pool_size">>, required, {integer}},
                    {<<"max_overflow">>, required, {integer}},
                    {<<"transaction_loglevel">>, required, {integer}}
                ]}},
                {<<"proto2">>, required, {hash, [
                    {<<"pool_size">>, required, {integer}},
                    {<<"max_overflow">>, required, {integer}},
                    {<<"transaction_loglevel">>, required, {integer}}
                ]}}
            ]}}
        ]}.



    -type jiffy_validate_ret() :: {ok, valid} | {ok, Value :: any()} | error().
    -type jiffy_fix_ret() :: {ok, Value :: any()} | error().



    -spec jiffy_validator(Type :: validate | fix, Path :: unicorn:path(), Value :: any()) ->
        jiffy_validate_ret() | jiffy_fix_ret().

    jiffy_validator(validate, [<<"database">>, <<"server">>], Value) ->
        %% mapping binary to erlang inet:ip4_address(); demonstration purposes only
        Octets = lists:map(fun(Octet) ->
            list_to_integer(binary_to_list(Octet))
        end, binary:split(Value, <<$.>>, [global])),
        IsFourOctets = 4 == length(Octets),
        IsOctetsBindingOk = lists:all(fun
            (Octet) when 0 < Octet, 256 > Octet -> true;
            (_Octet) -> false
        end, Octets),
        if
            IsFourOctets, IsOctetsBindingOk ->
                {ok, list_to_tuple(Octets)};
            true ->
                {error, <<"Invalid IP address format">>}
        end;

    jiffy_validator(validate, [<<"binding">>, _, <<"ports">>, _], Value) ->
        ?DBG("Ports: ~p", [Value]),
        if
            Value < 65536, Value > 0 ->
                {ok, valid};
            true ->
                {error, <<"Invalid protocol binding port">>}
        end;

    jiffy_validator(validate, [<<"database">>, <<"port">>], Value) ->
        if
            Value < 65536, Value > 0 ->
                {ok, valid};
            true ->
                {error, <<"Invalid database port">>}
        end;

    jiffy_validator(validate, [<<"worker">>, _, <<"transaction_loglevel">>], Value) ->
        if
            Value < 10, Value > 0 ->
                {ok, valid};
            true ->
                {error, <<"Invalid transactions loglevel">>}
        end;

    jiffy_validator(validate, _, _) ->
        {ok, valid};

    jiffy_validator(fix, Path, Value) ->
        ?DBG("FIX ~p -> ~p", [Path, Value]),
        {error, invalid}.



-endif.

