-module(test).

-behaviour(hera_measure).

-export([init/1, measure/1]).

init(_) ->
    Spec = #{
        name => nav,
        iter => infinity,
        timeout => 1000
    },
    {ok, 2, Spec}.


measure(State) when State > 0 ->
    io:format("~p~n", [State]),
    {ok, [0], State-1};

measure(0) ->
    true = false,
    {ok, [0], 0}.