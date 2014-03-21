-module(unicorn_worker).

-behaviour(gen_server).

-include("unicorn.hrl").



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



handle_call(?SUBSCRIBE(Pid, Path), _From, #state{subscribers = Subscribers} = State) ->
    ?DBG("~p subscribed for ~p:~p", [Pid, State#state.procname, Path]),
    Ref = erlang:monitor(process, Pid),
    NewState = State#state{
        subscribers = Subscribers ++ [{Pid, Path, Ref}]
    },
    {reply, ok, NewState};

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
    {Reply, NewState} = case load_document(File) of
        {ok, NewDocument} ->
            Diff = do_diff(Document, NewDocument),
            NumNotified = do_notify(Diff, Subscribers),
            ?DBG("~p notified ~p subscribers", [State#state.procname, NumNotified]),
            {{ok, NumNotified}, #state{
                document = Document
            }};
        {error, Reason} ->
            ?DBG("~p got error on reload: ", [State#state.procname, Reason]),
            {{error, Reason}, State}
    end,
    {reply, Reply, NewState};

handle_call(?LIST_SUBSCRIBERS, _From, #state{subscribers = Subscribers} = State) ->
    {reply, Subscribers, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.



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
        {ok, Document} ->
            Type = detect_file_type(File),
            parse_document(File, Type, Document);
        {error, Reason} ->
            {error, {unable_to_read, File, Reason}}
    end.



parse_document(File, etoml, Document) ->
    case etoml:parse(Document) of
        {error, Reason} ->
            {error, {unable_to_parse, File, Reason}};
        {ok, Contents} ->
            {ok, Contents}
    end;

parse_document(File, unknown, _Document) ->
    {error, {unable_to_parse, File, unknown_type}}.



detect_file_type(File) ->
    case filename:extension(File) of
        <<".toml">> ->
            etoml;
        _ ->
            unknown
    end.



do_diff(_Document, _NewDocument) ->
    [].



do_notify(_Diff, _Subscribers) ->
    0.



%% ~Internals