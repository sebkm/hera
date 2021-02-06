-module(hera_data).

-behaviour(gen_server).

-include("hera.hrl").

-export([start_link/0]).
-export([get/1, get/2, get/3]).
-export([store/3]).
-export([get_timestamp/0]).
-export([init/1, handle_call/3, handle_cast/2]).

-type measurement() :: {node(), measure(), timestamp()}.

-export_type([measure/0, measurement/0]).

-record(data, {
    measure = #measure{} :: measure(),
    timestamp :: timestamp(),
    file :: file:io_device() | undefined
}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


-spec get(Name) -> Measurements when
    Name :: atom(),
    Measurements :: [measurement()].

get(Name) ->
    gen_server:call(?MODULE, {get, Name}).


-spec get(Name, Node) -> {ok, Measurement} | undefined when 
    Name :: atom(),
    Node :: node(),
    Measurement :: measurement().

get(Name, Node) -> 
    gen_server:call(?MODULE, {get, Name, Node}).


%% get the data from measurement identified by Name and satisfying Filter
%% eliminate multiple candidates using CmpFun according to ordering
%% if CmpFun(A, B) then A is selected
-spec get(Name, Filter, CmpFun) -> {ok, Measurement} | undefined when
    Name :: atom(),
    Filter :: fun( (M) -> boolean() ),
    CmpFun :: fun( (M, M) -> boolean() ),
    Measurement :: M,
    M :: measurement().

get(Name, Filter, CmpFun) ->
    Candidates = lists:filter(Filter, hera_data:get(Name)),
    case lists:sort(CmpFun, Candidates) of
        [H|_] -> {ok, H};
        [] -> undefined
    end.


-spec store(Name, Node, Measure) -> ok when
    Name :: atom(),
    Node :: node(),
    Measure :: measure().

store(Name, Node, Measure) ->
    gen_server:cast(?MODULE, {store, Name, Node, Measure}).


-spec get_timestamp() -> timestamp().

get_timestamp() ->
  erlang:monotonic_time(millisecond).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([]) ->
    {ok, #{}}.


handle_call({get, Name}, _From, MapData) ->
    MapMeasure = maps:get(Name, MapData, #{}),
    L = maps:to_list(MapMeasure),
    Res = [{Node, M, T} || {Node, #data{measure=M, timestamp=T}} <- L],
    {reply, Res, MapData};

handle_call({get, Name, Node}, _From, MapData) ->
    MapMeasure = maps:get(Name, MapData, #{}),
    Res = if
        is_map_key(Node, MapMeasure) ->
            #data{measure=M, timestamp=T} = maps:get(Node, MapMeasure),
            {ok, {Node, M, T}};
        true ->
            undefined
    end,
    {reply, Res, MapData};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast({store, Name, Node, Measure}, State) ->
    {noreply, store(Name, Node, Measure, State)};
handle_cast(_Request, State) ->
    {noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

store(Name, Node, Measure=#measure{seq=Seq1}, MapData) ->
    MapNode0 = maps:get(Name, MapData, #{}),
    IsLogger = application:get_env(hera, log_data, false),
    MapNode1 = if
        is_map_key(Node, MapNode0) ->
            MapNode0;
        IsLogger ->
            File = open_file(Name, Node),
            #{Node => #data{file=File}};
        true ->
            #{Node => #data{}}
    end,
    Data = maps:get(Node, MapNode1),
    MapNode2 = case Data of
        #data{measure=M} when M#measure.seq < Seq1 ->
            log_data(Data#data.file, Measure, IsLogger),
            NewData = Data#data{measure=Measure, timestamp=get_timestamp()},
            maps:put(Node, NewData, MapNode1);
        _ ->
            MapNode1
    end,
    maps:put(Name, MapNode2, MapData).


open_file(Name, Node) ->
    FileName = lists:append(
        ["measures/", atom_to_list(Name), "_", atom_to_list(Node), ".csv"]),
    ok = filelib:ensure_dir("measures/"),
    {ok, File} = file:open(FileName, [append]),
    File.


log_data(_, _, false) ->
    ok;
log_data(File, #measure{seq=Seq, timestamp=T, values=Ms}, true) ->
    Vals = lists:map(fun(V) -> lists:flatten(io_lib:format("~p", [V])) end, Ms),
    S = string:join(Vals, ","),
    io:format(File, "~p,~p,~s~n", [Seq, T, S]).
