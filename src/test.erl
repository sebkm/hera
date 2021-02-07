-module(test).

-behaviour(hera_measure).

-export([init/1, measure/1]).

init(_) ->
    Spec = #{
        name => nav,
        iter => 3,
        timeout => 1000,
        sync => true
    },
    {ok, 10, Spec}.


measure(State) ->
    timer:sleep(1000),
    {ok, [0], State-1}.