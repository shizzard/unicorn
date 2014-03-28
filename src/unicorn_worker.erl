-module(unicorn_worker).

-behaviour(gen_server).

-include("unicorn.hrl").
-include("unicorn_client.hrl").



%% API
-export([start_link/3]).

%% gen_server callbacks
-export([
    init/1, handle_call/3, handle_cast/2,
    handle_info/2, terminate/2, code_change/3
]).



-type subscriber() :: {pid(), unicorn:path(), reference()}.

-record(state, {
    file :: unicorn:filename(),
    loader :: unicorn:loader(),
    validator :: unicorn:validator(),
    procname :: atom(),
    document :: unicorn:document(),
    subscribers = [] :: list(subscriber())
}).



%% Interface



-spec start_link(File :: unicorn:filename(), Loader :: unicorn:loader(), Validator :: unicorn:validator()) ->
    {ok, Pid :: pid()} | ignore | {error, Error :: any()}.
start_link(File, Loader, Validator) ->
    ProcName = ?FILE_TO_NAME(File),
    ?DBG("~p started for '~p' file", [ProcName, File]),
    gen_server:start_link({local, ProcName}, ?MODULE, [File, ProcName, Loader, Validator], []).



-spec init(Args :: list()) ->
    {ok, State :: #state{}} | {error, Error :: any()}.
init([File, ProcName, Loader, Validator]) ->
    try
        {ok, Document} = load_document(File, Loader, Validator),
        {ok, #state{
            file = File,
            loader = Loader,
            validator = Validator,
            procname = ProcName,
            document = Document
        }}
    catch error:{error, Reason} ->
        {stop, Reason}
    end.



-spec handle_call(Message :: any(), From :: pid(), State :: #state{}) ->
    {reply, Reply :: any(), State :: #state{}}.
handle_call(?SUBSCRIBE(Pid, Path), _From, State) ->
    case  unicorn:get(Path, State#state.document) of
        {error, undefined} ->
            {reply, {error, not_found}, State};
        {ok, Value} ->
            ?DBG("~p subscribed for ~p:~p", [Pid, State#state.procname, Path]),
            Ref = erlang:monitor(process, Pid),
            NewState = State#state{
                subscribers = State#state.subscribers ++ [{Pid, Path, Ref}]
            },
            {reply, {ok, Value}, NewState}
    end;

handle_call(?UNSUBSCRIBE(Pid), _From, State) ->
    ?DBG("~p unsubscribed for ~p", [Pid, State#state.procname]),
    NewState = State#state{
        subscribers = lists:filter(fun
            ({Pid0, _Path0, Ref0}) when Pid0 == Pid ->
                erlang:demonitor(Ref0),
                false;
            ({_, _, _}) ->
                true
        end, State#state.subscribers)
    },
    {reply, ok, NewState};

handle_call(?UNSUBSCRIBE(Pid, Path), _From, State) ->
    ?DBG("~p unsubscribed for ~p:~p", [Pid, State#state.procname, Path]),
    NewState = State#state{
        subscribers = lists:filter(fun
            ({Pid0, Path0, Ref0}) when Pid0 == Pid, Path0 == Path ->
                erlang:demonitor(Ref0),
                false;
            ({_, _, _}) ->
                true
        end, State#state.subscribers)
    },
    {reply, ok, NewState};

handle_call(?RELOAD, _From, State) ->
    ?DBG("~p received reload signal", [State#state.procname]),
    {Reply, NewState} = try
        {ok, NewDocument} = load_document(State#state.file, State#state.loader, State#state.validator),
        NumNotified = case do_diff(State#state.file, State#state.document, NewDocument) of
            {ok, Diff} ->
                ?DBG("Diff for file ~p: ~p", [State#state.file, Diff]),
                do_notify(State#state.file, Diff, NewDocument, State#state.subscribers);
            {error, empty} ->
                0
        end,
        ?DBG("~p notified ~p subscribers", [State#state.procname, NumNotified]),
        {{ok, NumNotified}, State#state{
            document = NewDocument
        }}
    catch error:{error, Reason} ->
            ?DBG("~p got error on reload: ~p", [State#state.procname, Reason]),
            {{error, Reason}, State}
    end,
    {reply, Reply, NewState};

handle_call(?LIST_SUBSCRIBERS, _From, State) ->
    {reply, State#state.subscribers, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.



-spec handle_cast(Message :: any(), State :: #state{}) ->
    {noreply, State :: #state{}} | {stop, normal, State :: #state{}}.
handle_cast(?TERMINATE, #state{file = File, subscribers = Subscribers} = State) ->
    lists:foreach(fun({Pid, Path, _Ref}) ->
        Pid ! ?UNICORN_TERMINATE(File, Path)
    end, Subscribers),
    {stop, normal, State};

handle_cast(_Msg, State) ->
    {noreply, State}.



-spec handle_info(Message :: any(), State :: #state{}) ->
    {noreply, State :: #state{}}.
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



-spec terminate(Reason :: any(), State :: #state{}) ->
    ok.
terminate(_Reason, _State) ->
    ok.



-spec code_change(OldVsn :: any(), State :: #state{}, Extra :: any()) ->
    {ok, State :: #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



%% ~Interface



%% Internals



-spec load_document(File :: unicorn:filename(), Loader :: unicorn:loader(), Validator :: unicorn:validator()) ->
    {ok, Document :: unicorn:document()}.
load_document(File, Loader, Validator) ->
    case file:read_file(File) of
        {ok, RawDocument} ->
            case Loader(RawDocument) of
                {ok, Document0} ->
                    case Validator(Document0) of
                        {ok, Document1} ->
                            {ok, Document1};
                        {error, ErrorList} ->
                            erlang:error({error, {unable_to_validate, File, ErrorList}})
                    end;
                {error, Reason} ->
                    erlang:error({error, {unable_to_load, File, Reason}})
            end;
        {error, Reason} ->
            erlang:error({error, {unable_to_read, File, Reason}})
    end.



-spec do_diff(File :: unicorn:filename(), Document :: unicorn:document(), NewDocument :: unicorn:document()) ->
    {ok, Document :: unicorn:document()} | {error, empty}.
do_diff(File, Document, NewDocument) ->
    do_diff(File, [], Document, NewDocument).

-spec do_diff(File :: unicorn:filename(), Path :: unicorn:path(), Document :: unicorn:document(), NewDocument :: unicorn:document()) ->
    Document :: unicorn:document().
do_diff(File, Path, {DocumentPL}, {NewDocumentPL}) ->
    Diff = lists:foldl(fun({Key, Value}, {Acc}) ->
        proplists:is_defined(Key, NewDocumentPL) orelse
            erlang:error({unexistent_key, File, Path ++ [Key]}),
        Neighbor = proplists:get_value(Key, NewDocumentPL),
        case do_diff(File, Path ++ [Key], Value, Neighbor) of
            {error, empty} -> {Acc};
            {ok, SubDiff} -> {Acc ++ [{Key, SubDiff}]}
        end
    end, {[]}, DocumentPL),
    case Diff of
        {[]} -> {error, empty};
        Diff -> {ok, Diff}
    end;

do_diff(_File, _Path, Document, NewDocument) when is_list(Document), is_list(NewDocument) ->
    case NewDocument -- Document of
        [] -> {error, empty};
        _Diff -> {ok, NewDocument}
    end;

do_diff(_File, _Path, Document, NewDocument) when Document == NewDocument ->
    {error, empty};

do_diff(_File, _Path, _Document, NewDocument) ->
    {ok, NewDocument}.



-spec do_notify(File :: unicorn:filename(), Diff :: unicorn:document(), Document :: unicorn:document(), Subscribers :: list(subscriber())) ->
    NumNotified :: integer().
do_notify(File, Diff, Document, Subscribers) ->
    lists:foldl(fun({Pid, Path, _Ref}, Acc) ->
        case unicorn:get(Path, Diff) of
            {error, undefined} ->
                Acc;
            {ok, SubDiff} ->
                {ok, SubDocument} = unicorn:get(Path, Document),
                Pid ! ?UNICORN_NOTIFY(File, Path, SubDiff, SubDocument),
                Acc + 1
        end
    end, 0, Subscribers).



%% ~Internals