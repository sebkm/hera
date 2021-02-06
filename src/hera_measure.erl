-module(hera_measure).

-behaviour(gen_server).

-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-include("hera.hrl").

-type measure_spec() :: #{
    name := atom(), % measure id
    iter := pos_integer() | infinity, % number of measures to perform
    sync => boolean(), % must the measure must be synchronized? (default: false)
    timeout => timeout(), % delay between two measures (default: 0)
    seq => pos_integer() % initial sequence number (default: 1)
}.

-export_type([measure_spec/0]).

-callback init(Args :: term()) ->
    {ok, State :: term(), Spec :: measure_spec()}.
-callback measure(State :: term()) ->
    {ok, Measure :: [number(), ...], NewState :: term()} | undefined.

-record(state, {
    name :: atom(),
    sync = false :: boolean(),
    timeout = 0 :: timeout(),
    seq = 1 :: pos_integer(),
    iter = 1 :: non_neg_integer() | infinity,
    mod :: module(),
    mod_state :: term()
}).

-define(record_to_tuplelist(Name, Rec),
    lists:zip(record_info(fields, Name), tl(tuple_to_list(Rec)))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link(Module, Args) ->
    gen_server:start_link(?MODULE, {Module, Args}, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init({Mod, Args}) ->
    {ok, ModState, Spec} = Mod:init(Args),
    L0 = ?record_to_tuplelist(state, #state{}),
    L1 = lists:map(fun({Key, Val}) -> maps:get(Key, Spec, Val) end, L0),
    State = list_to_tuple([state|L1]),
    Seq = init_seq(State#state.name, State#state.seq),
    NewState = State#state{seq=Seq, mod=Mod, mod_state=ModState},
    {ok, NewState, State#state.timeout}.


handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast(_Request, State) ->
    {noreply, State}.


handle_info(timeout, State) ->
    NewState = measure(State),
    case NewState of
        #state{iter=0} ->
            {stop, normal, NewState};
        _ ->
            {noreply, NewState, NewState#state.timeout}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% return Seq or the last known seq number (S0) + 1 if S0 > Seq
init_seq(Name, Seq) ->
    case hera_data:get(Name, node()) of
        {ok, {_, #measure{seq=Seq0}, _}} when Seq0 > Seq ->
            Seq0+1;
        _ ->
            Seq
    end.


measure(State=#state{name=N, mod=M, mod_state=MS, seq=Seq, iter=Iter}) ->
    case M:measure(MS) of
        undefined ->
            State;
        {ok, Vals=[_|_], NewMS} ->
            Measure = #measure{seq=Seq, timestamp=hera_data:get_timestamp(),
                values = Vals},
            hera_com:send(N, Measure),
            NewIter = case Iter of
                infinity -> Iter;
                _ -> Iter-1
            end,
            State#state{seq=Seq+1, iter=NewIter, mod_state=NewMS}
    end.
