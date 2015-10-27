-module(unicorn_nofile_worker).

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
    validator :: unicorn:validator(),
    procname :: unicorn:procname(),
    document :: unicorn:document(),
    subscribers = [] :: list(subscriber())
}).



%% Interface



-spec start_link(ProcName :: unicorn:procname(), Document :: unicorn:document(), Validator :: unicorn:validator()) ->
    {ok, Pid :: pid()} | ignore | {error, Error :: any()}.
start_link(ProcName, Document, Validator) ->
    ?DBG("~p started", [ProcName]),
    gen_server:start_link({local, ProcName}, ?MODULE, [ProcName, Document, Validator], []).



-spec init(Args :: term()) ->
    {ok, State :: #state{}} |
    {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} |
    ignore.
init([ProcName, Document0, Validator]) ->
    try
        {ok, Document} = load_document(ProcName, Document0, Validator),
        {ok, #state{
            validator = Validator,
            procname = ProcName,
            document = Document
        }}
    catch error:{error, Reason} ->
        {stop, Reason}
    end.



-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}} |
    {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
    {stop, Reason :: term(), NewState :: #state{}}.
handle_call(?SUBSCRIBE(_Pid, _Path), _From, State) ->
    {reply, not_implemented, State};

handle_call(?UNSUBSCRIBE(_Pid), _From, State) ->
    {reply, not_implemented, State};

handle_call(?UNSUBSCRIBE(_Pid, _Path), _From, State) ->
    {reply, not_implemented, State};

handle_call(?RELOAD, _From, State) ->
    {reply, not_implemented, State};

handle_call(?GET(Path), _From, State) ->
    case unicorn:get_path(Path, State#state.document) of
        {error, undefined} ->
            {reply, {error, not_found}, State};
        {ok, Value} ->
            {reply, {ok, Value}, State}
    end;

handle_call(?LIST_SUBSCRIBERS, _From, State) ->
    {reply, not_implemented, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.



-spec handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}.
handle_cast(?TERMINATE, #state{procname = ProcName, subscribers = Subscribers} = State) ->
    lists:foreach(fun({Pid, Path, _Ref}) ->
        Pid ! ?UNICORN_TERMINATE(ProcName, Path)
    end, Subscribers),
    {stop, normal, State};

handle_cast(_Msg, State) ->
    {noreply, State}.



-spec handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}.
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



-spec load_document(ProcName :: unicorn:procname(), Document0 :: unicorn:document(), Validator :: unicorn:validator()) ->
    {ok, Document :: unicorn:document()}.
load_document(ProcName, Document0, Validator) ->
    case Validator(Document0) of
        {ok, Document1} ->
            {ok, Document1};
        {error, ErrorList} ->
            erlang:error({error, {unable_to_validate, ProcName, ErrorList}})
    end.



%% ~Internals