-module(hera_data).

-behaviour(gen_server).

-include("hera.hrl").

-export([start_link/0]).
-export([get_data/1]).
-export([store_data/3]).
-export([get_timestamp/0]).

-export([init/1, handle_call/3, handle_cast/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


get_data(Name) ->
    gen_server:call(?MODULE, {get_data, Name}).


store_data(Name, Node, Data) ->
    gen_server:cast(?MODULE, {store_data, Name, Node, Data}).


get_timestamp() ->
  erlang:monotonic_time(millisecond).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([]) ->
    {ok, #{}}.


handle_call({get_data, Name}, _From, State) ->
    {reply, get_data(Name, State), State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast({store_data, Name, Node, Data}, State) ->
    {noreply, store_data(Name, Node, Data, State)};
handle_cast(_Request, State) ->
    {noreply, State}.


terminate(_Reason, State) ->
    undefined. % TODO: close files

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_data(Name, MapData) ->
    MapMeasure = maps:get(Name, MapData, #{}),
    L = maps:to_list(MapMeasure),
    [{Node, Measure} || {Node, {Measure, _}} <- L].


store_data(Name, Node, Measure=#measure{seq=Seq1}, MapData) ->
    MapMeasure0 = maps:get(Name, MapData, #{}),
    Measure0 = maps:get(Node, MapMeasure0, {#measure{}, undefined}),
    MapMeasure1 = case Measure0 of
        {#measure{seq=Seq0}, _} when Seq0 < Seq1 ->
            log_data(Measure),
            maps:put(Node, {Measure, get_timestamp()}, MapMeasure0);
        _ ->
            MapMeasure0
    end,
    maps:put(Name, MapMeasure1, MapData).


log_data(Measure) -> % TODO
    io:format("~p~n", [Measure]).
