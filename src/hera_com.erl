-module(hera_com).

-export([start_link/0]).
-export([send/1]).

-define(MULTICAST_ADDR, {224,0,2,15}).
-define(MULTICAST_PORT, 62476).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link() ->
    Pid = spawn_link(fun init/0),
    register(?MODULE, Pid),
    {ok, Pid}.


send(Message) ->
    try ?MODULE ! {send_message, term_to_binary(Message)}
    catch
        error:_ -> ok
    end,
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init() ->
    {ok, Addrs} = inet:getifaddrs(),
    OwnAddr = hd([
        Addr || {_, Opts} <- Addrs, {addr, Addr} <- Opts,
        size(Addr) == 4, Addr =/= {127,0,0,1}
    ]),
    {ok, Socket} = gen_udp:open(?MULTICAST_PORT, [
        binary,
        inet,
        {active, true},
        {multicast_if, OwnAddr},
        {multicast_loop, true}, % TODO: set to false ?
        {reuseaddr, true},
        {add_membership, {?MULTICAST_ADDR, OwnAddr}}
    ]),
    loop(Socket).


loop(Socket) ->
    receive
        {udp, _Sock, _IP, _InPortNo, Packet} ->
            Message = binary_to_term(Packet),
            io:format("Received: ~p~n", [Message]); % TODO: send to hera_data
        {send_message, Message} ->
            gen_udp:send(Socket, ?MULTICAST_ADDR, ?MULTICAST_PORT, Message);
        _ ->
            ok
    end,
    loop(Socket).
