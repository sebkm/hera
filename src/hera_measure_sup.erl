-module(hera_measure_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([start_child/2]).

-export([init/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


start_child(Module, Args) ->
    supervisor:start_child(?MODULE, [Module, Args]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([]) ->
    SupFlags = #{
        strategy => simple_one_for_one,
        intensity => 10,
        period => 60
    },
    HeraMeasure = #{
        id => hera_measure,
        start => {hera_measure, start_link, []},
        restart => transient
    },
    ChildSpecs = [HeraMeasure],
    {ok, {SupFlags, ChildSpecs}}.
