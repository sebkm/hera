-module(hera_measure).

-export([start_link/2]).

-type measure_spec() :: #{
    name := atom(), % measure id
    iter := pos_integer() | infinity, % number of measures to perform
    sync => boolean(), % must the measure must be synchronized? (default: false)
    timeout => timeout() % min delay between two measures (default: 1)
}.

-export_type([measure_spec/0]).

-callback init(Args :: term()) ->
    {ok, State :: term(), Spec :: measure_spec()}.

-callback measure(State :: term()) ->
    {ok, Values, NewState} | {undefined, NewState} when
    Values :: [number(), ...],  
    NewState :: term().

-record(state, {
    name :: atom(),
    sync = false :: boolean(),
    monitor :: {pid(), reference()} | undefined,
    timeout = 1 :: timeout(),
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
    Pid = spawn_link(fun() -> init({Module, Args}) end),
    {ok, Pid}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init({Mod, Args}) ->
    {ok, ModState, Spec} = Mod:init(Args),
    L0 = ?record_to_tuplelist(state, #state{}),
    L1 = lists:map(fun({Key, Val}) -> maps:get(Key, Spec, Val) end, L0),
    State = list_to_tuple([state|L1]),
    Seq = init_seq(State#state.name),
    case State#state.sync of
        true ->
            PidRef = subscribe(State#state.name),
            NewState =
                State#state{seq=Seq,mod=Mod,mod_state=ModState,monitor=PidRef},
            loop(NewState, true);
        false ->
            NewState = State#state{seq=Seq,mod=Mod,mod_state=ModState},
            loop(NewState, false)
    end.


loop(State, false) ->
    continue(measure(State));
loop(State=#state{monitor={From,Ref}}, true) ->
    receive
        {authorized, From} ->
            NewState = measure(State),
            From ! {ok, self()},
            continue(NewState);
        {'DOWN', Ref, _, _, _} ->
            PidRef = subscribe(State#state.name),
            continue(State#state{monitor=PidRef})
    end.


continue(#state{iter=0}) ->
    {stop, normal};
continue(State) ->
    timer:sleep(State#state.timeout),
    loop(State, State#state.sync).


subscribe(Name) ->
    {ok, Pid} = hera_sub:subscribe(Name),
    Ref = monitor(process, Pid),
    {Pid, Ref}.


%% return 1 or 1 + the last seq number known among all nodes
init_seq(Name) ->
    {ResL, _} = rpc:multicall(hera_data, get, [Name, node()]),
    L = lists:filtermap(fun(Res) ->
        case Res of
            [{_,Seq,_,_}] -> {true, Seq};
            _ -> false
        end
    end, ResL),
    lists:max([0|L]) + 1.


measure(State=#state{name=N, mod=M, mod_state=MS, seq=Seq, iter=Iter}) ->
    case M:measure(MS) of
        {undefined, NewMS} ->
            State#state{mod_state=NewMS};
        {ok, Vals=[_|_], NewMS} ->
            hera_com:send(N, Seq, Vals),
            NewIter = case Iter of
                infinity -> Iter;
                _ -> Iter-1
            end,
            State#state{seq=Seq+1, iter=NewIter, mod_state=NewMS}
    end.
