-module(hera_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link() ->
    supervisor:start_link(?MODULE, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 6,
        period => 3600
    },
    HeraData = #{
        id => hera_data,
        start => {hera_data, start_link, []}
    },
    HeraCom = #{
        id => hera_com,
        start => {hera_com, start_link, []}
    },
    HeraMeasureSup = #{
        id => hera_measure_sup,
        start => {hera_measure_sup, start_link, []},
        type => supervisor
    },
    ChildSpecs = [HeraData, HeraCom, HeraMeasureSup],
    {ok, {SupFlags, ChildSpecs}}.
