-module(test).

-behaviour(hera_measure).

-export([init/1, measure/1]).

init(_) ->
    Spec = #{
        name => nav,
        iter => infinity,
        timeout => 1000
    },
    {ok, 10, Spec}.


measure(State) ->
    io:format("~p~n", [State]),
    {ok, [0], State-1}.