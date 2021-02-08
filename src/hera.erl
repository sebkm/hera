-module(hera).

-behaviour(application).

-export([start_measure/2, timestamp/0]).
-export([start/2, stop/1]).

-type timestamp() :: integer() | undefined.

-export_type([timestamp/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% starts and supervise the measure defined in the callback module
%% using Module:init(Args)
-spec start_measure(Module, Args) -> {ok, pid()} | {error, term()} when
    Module :: module(),
    Args :: term().

start_measure(Module, Args) ->
    hera_measure_sup:start_child(Module, Args).


-spec timestamp() -> timestamp().

timestamp() ->
  erlang:monotonic_time(millisecond).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start(_StartType, _StartArgs) ->
    hera_sup:start_link().


stop(_State) ->
    ok.
