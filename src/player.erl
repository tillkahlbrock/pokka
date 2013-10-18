-module(player).

-export([start/1]).
-record(state, {
  name="",
  pocketCards=""
}).

%%%===================================================================
%%% API
%%%===================================================================
-spec(start /1:: (string()) -> pid()).
start(Name) -> spawn(fun() -> play(#state{name=Name}) end).


%%%===================================================================
%%% Internal functions
%%%===================================================================
play(#state{name=Name}) ->
  Socket = connect(),
  send(Socket, "JOIN " ++ Name),
  _PocketCards = wait_for_pocketcards(),
  wait_for_blind(Socket).

connect() ->
  {ok, Socket} = gen_tcp:connect('localhost', 12345, [binary, {packet, 0}]),
  Socket.

wait_for_pocketcards() ->
  receive
    {tcp, _Socket, <<"POCKETCARDS ", Binary/binary>>} ->
      string:strip(binary_to_list(Binary), right, $\n)
  end.

wait_for_blind(Socket) ->
  receive
    {tcp, Socket, <<"BIGBLIND ", BigBinary/binary>>} ->
      BigAmount = string:strip(string:strip(binary_to_list(BigBinary), right, $\n), right, $\r),
      send(Socket, "BIGBLIND " ++ BigAmount);

    {tcp, Socket, <<"SMALLBLIND ", SmallBinary/binary>>} ->
      SmallAmount = string:strip(string:strip(binary_to_list(SmallBinary), right, $\n), right, $\r),
      send(Socket, "SMALLBLIND " ++ SmallAmount)
  end.

send(Socket, Message) ->
  gen_tcp:send(Socket, Message ++ "\r\n").
