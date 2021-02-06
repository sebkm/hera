-module(hera_com).

-export([start_link/0]).
-export([send/2]).

-define(MULTICAST_ADDR, {224,0,2,15}).
-define(MULTICAST_PORT, 62476).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link() ->
    Pid = spawn_link(fun init/0),
    register(?MODULE, Pid),
    {ok, Pid}.


-spec send(Name, Measure) -> ok when
    Name :: atom(),
    Measure :: {pos_integer(), [number(), ...]}.

send(Name, Measure) ->
    Message = {hera_data, Name, node(), Measure},
    try ?MODULE ! {send_packet, term_to_binary(Message)}
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
            case Message of
                {hera_data, Name, From, Measure} ->
                    hera_data:store(Name, From, Measure);
                _ ->
                    ok
            end;
        {send_packet, Packet} ->
            gen_udp:send(Socket, ?MULTICAST_ADDR, ?MULTICAST_PORT, Packet);
        _ ->
            ok
    end,
    loop(Socket).
