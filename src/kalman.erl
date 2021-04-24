-module(kalman).

-export([kf_predict/3, kf_update/4]).
-export([kf/6, ekf/6]).

%% see https://en.wikipedia.org/wiki/Kalman_filter


%% A kalman filter without control input
kf({X0, P0}, F, H, Q, R, Z) ->  
    {Xp, Pp} = kf_predict({X0, P0}, F, Q),
    kf_update({Xp, Pp}, H, R, Z).


kf_predict({X0, P0}, F, Q) ->
    Xp = mat:'*'(F, X0), 
    Pp = mat:eval([F, '*', P0, '*´', F, '+', Q]),
    {Xp, Pp}.


kf_update({Xp, Pp}, H, R, Z) ->
    S = mat:eval([H, '*', Pp, '*´', H, '+', R]), 
    Sinv = mat:inv(S), 
    K = mat:eval([Pp, '*´', H, '*', Sinv]),
    Y = mat:'-'(Z, mat:'*'(H, Xp)),
    X1 = mat:eval([K, '*', Y, '+', Xp]),  
    P1 = mat:'-'(Pp, mat:eval([K, '*', H, '*', Pp])), 
    {X1, P1}.


%% An extended kalman filter without control input
ekf({X0, P0}, {F, Jf}, {H, Jh}, Q, R, Z) ->
    % Prediction
    Xp = F(X0),
    Jfx = Jf(X0),
    Pp = mat:eval([Jfx, '*', P0, '*´', Jfx, '+', Q]),

    % Update
    Jhx = Jh(Xp),
    S = mat:eval([Jhx, '*', Pp, '*´', Jhx, '+', R]),
    Sinv = mat:inv(S),
    K = mat:eval([Pp, '*´', Jhx, '*', Sinv]),
    Y = mat:'-'(Z, H(Xp)),
    X1 = mat:eval([K, '*', Y, '+', Xp]),
    P1 = mat:'-'(Pp, mat:eval([K, '*', Jhx, '*', Pp])),
    {X1, P1}.