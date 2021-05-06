-module(hera_com).

-export([start_link/0]).
-export([send/3]).

-define(MULTICAST_ADDR, {224,0,2,15}).
-define(MULTICAST_PORT, 62476).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link() ->
    Pid = spawn_link(fun init/0),
    register(?MODULE, Pid),
    {ok, Pid}.


-spec send(Name, Seq, Values) -> ok when
    Name :: atom(),
    Seq :: pos_integer(),
    Values :: [number(), ...].

send(Name, Seq, Values) ->
    Message = {hera_data, Name, node(), Seq, Values},
    try ?MODULE ! {send_packet, term_to_binary(Message)}
    catch
        error:_ -> ok
    end,
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init() ->
    Socket = open_socket(1),
    io:format("Connection established!~n"),
    loop(Socket).


open_socket(Delay) ->
    try open_socket()
    catch
        error:Reason ->
            io:format("Could not open socket:~p~n", [Reason]),
            io:format("Retrying in ~p [s]~n", [Delay]),
            timer:sleep(Delay*1000),
            open_socket(min(2*Delay, 60))
    end.


open_socket() ->
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
        {multicast_loop, true},
        {reuseaddr, true},
        {add_membership, {?MULTICAST_ADDR, OwnAddr}}
    ]),
    Socket.


loop(Socket) ->
    receive
        {udp, _Sock, _IP, _InPortNo, Packet} ->
            Message = binary_to_term(Packet),
            case Message of
                {hera_data, Name, From, Seq, Values} ->
                    hera_data:store(Name, From, Seq, Values);
                _ ->
                    ok
            end;
        {send_packet, Packet} ->
            gen_udp:send(Socket, ?MULTICAST_ADDR, ?MULTICAST_PORT, Packet);
        _ ->
            ok
    end,
    loop(Socket).
