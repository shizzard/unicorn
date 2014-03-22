-module(unicorn_worker).

-behaviour(gen_server).

-include("unicorn.hrl").
-include("unicorn_client.hrl").



%% API
-export([start_link/1]).



%% gen_server callbacks
-export([
    init/1, handle_call/3, handle_cast/2,
    handle_info/2, terminate/2, code_change/3
]).

-record(state, {
    file :: binary(),
    procname :: atom(),
    document :: list(),
    subscribers = [] :: list()
}).



%% Interface



start_link(File) ->
    ProcName = ?FILE_TO_NAME(File),
    ?DBG("~p started for '~p' file", [ProcName, File]),
    gen_server:start_link({local, ProcName}, ?MODULE, [File, ProcName], []).



init([File, ProcName]) ->
    case load_document(File) of
        {ok, Document} ->
            {ok, #state{
                file = File,
                procname = ProcName,
                document = Document
            }};
        {error, Reason} ->
            {stop, Reason}
    end.



handle_call(?SUBSCRIBE(Pid, Path), _From, #state{document = Document, subscribers = Subscribers} = State) ->
    case  unicorn:get(Path, Document) of
        undefined ->
            {reply, {error, not_found}, State};
        Value ->
            ?DBG("~p subscribed for ~p:~p", [Pid, State#state.procname, Path]),
            Ref = erlang:monitor(process, Pid),
            NewState = State#state{
                subscribers = Subscribers ++ [{Pid, Path, Ref}]
            },
            {reply, {ok, Value}, NewState}
    end;

handle_call(?UNSUBSCRIBE(Pid), _From, #state{subscribers = Subscribers} = State) ->
    ?DBG("~p unsubscribed for ~p", [Pid, State#state.procname]),
    NewState = State#state{
        subscribers = lists:filter(fun
            ({Pid0, _Path0, Ref0}) when Pid0 == Pid ->
                erlang:demonitor(Ref0),
                false;
            ({_, _, _}) ->
                true
        end, Subscribers)
    },
    {reply, ok, NewState};

handle_call(?UNSUBSCRIBE(Pid, Path), _From, #state{subscribers = Subscribers} = State) ->
    ?DBG("~p unsubscribed for ~p:~p", [Pid, State#state.procname, Path]),
    NewState = State#state{
        subscribers = lists:filter(fun
            ({Pid0, Path0, Ref0}) when Pid0 == Pid, Path0 == Path ->
                erlang:demonitor(Ref0),
                false;
            ({_, _, _}) ->
                true
        end, Subscribers)
    },
    {reply, ok, NewState};

handle_call(?RELOAD, _From, #state{file = File, document = Document, subscribers = Subscribers} = State) ->
    ?DBG("~p received reload signal", [State#state.procname]),
    {Reply, NewState} = try
        {ok, NewDocument} = load_document(File),
        Diff = do_diff(File, Document, NewDocument),
        ?DBG("Diff for file ~p: ~p", [File, Diff]),
        NumNotified = do_notify(File, Diff, NewDocument, Subscribers),
        ?DBG("~p notified ~p subscribers", [State#state.procname, NumNotified]),
        {{ok, NumNotified}, State#state{
            document = NewDocument
        }}
    catch error:{error, Reason} ->
            ?DBG("~p got error on reload: ", [State#state.procname, Reason]),
            {{error, Reason}, State}
    end,
    {reply, Reply, NewState};

handle_call(?LIST_SUBSCRIBERS, _From, #state{subscribers = Subscribers} = State) ->
    {reply, Subscribers, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.



handle_cast(?TERMINATE, #state{file = File, subscribers = Subscribers} = State) ->
    lists:foreach(fun({Pid, Path, _Ref}) ->
        Pid ! ?UNICORN_TERMINATE(File, Path)
    end, Subscribers),
    {stop, normal, State};

handle_cast(_Msg, State) ->
    {noreply, State}.



handle_info({'DOWN', Ref, process, Pid, _Info}, #state{subscribers = Subscribers} = State) ->
    ?DBG("~p got subscriber ~p down: ~p", [State#state.procname, Pid, _Info]),
    NewState = State#state{
        subscribers = lists:filter(fun
            ({Pid0, _Path0, Ref0}) when Pid0 == Pid, Ref0 == Ref ->
                false;
            ({_, _, _}) ->
                true
        end, Subscribers)
    },
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.



terminate(_Reason, _State) ->
    ok.



code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



%% ~Interface



%% Internals



load_document(File) ->
    case file:read_file(File) of
        {ok, RawDocument} ->
            Type = detect_file_type(File),
            parse_document(File, Type, RawDocument);
        {error, Reason} ->
            erlang:error({error, {unable_to_read, File, Reason}})
    end.



parse_document(File, etoml, RawDocument) ->
    case etoml:parse(RawDocument) of
        {ok, Document} ->
            {ok, Document};
        {error, Reason} ->
            erlang:error({error, {unable_to_parse, File, Reason}})
    end;

parse_document(File, unknown, _Document) ->
    erlang:error({error, {unable_to_parse, File, unknown_type}}).



detect_file_type(File) ->
    case filename:extension(File) of
        <<".toml">> ->
            etoml;
        _ ->
            unknown
    end.



do_diff(File, Document, NewDocument) ->
    do_diff(File, [], Document, NewDocument).

do_diff(File, Path, Document, NewDocument) ->
    IsProplist = is_proplist(Document),
    if
        IsProplist ->
            do_diff_proplist(File, Path, Document, NewDocument);
        is_list(Document) ->
            do_diff_list(File, Path, Document, NewDocument);
        true ->
            do_diff_etc(File, Path, Document, NewDocument)
    end.



do_diff_proplist(File, Path, Document, NewDocument) ->
    is_proplist(Document) andalso is_proplist(NewDocument) orelse
        erlang:error({invalid_proplist, File, Path}),
    lists:foldl(fun({Key, Value}, Acc) ->
        proplists:is_defined(Key, NewDocument) orelse
            erlang:error({unexistent_key, File, Path ++ [Key]}),
        Neighbor = proplists:get_value(Key, NewDocument),
        case do_diff(File, Path ++ [Key], Value, Neighbor) of
            [] -> Acc;
            SubDiff -> Acc ++ [{Key, SubDiff}]
        end
    end, [], Document).



do_diff_list(_File, _Path, Document, NewDocument) when is_list(Document), is_list(NewDocument) ->
    NewDocument;

do_diff_list(File, Path, _Document, _NewDocument) ->
    erlang:error({invalid_list, File, Path}).



do_diff_etc(_File, _Path, Document, NewDocument) when Document =:= NewDocument ->
    [];

do_diff_etc(_File, _Path, _Document, NewDocument) ->
    NewDocument.



do_notify(File, Diff, Document, Subscribers) ->
    lists:foldl(fun({Pid, Path, _Ref}, Acc) ->
        case unicorn:get(Path, Diff) of
            undefined ->
                Acc;
            Value ->
                Pid ! ?UNICORN_NOTIFY(File, Path, Value, unicorn:get(Path, Document)),
                Acc + 1
        end
    end, 0, Subscribers).



is_proplist(Item) when is_list(Item) ->
    lists:all(fun
        ({_, _}) -> true;
        (_) -> false
    end, Item);

is_proplist(_Item) ->
    false.



%% ~Internals