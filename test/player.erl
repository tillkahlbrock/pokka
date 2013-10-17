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
play(State = #state{name=Name}) ->
  Socket = connect(),
  send(Socket, "JOIN " ++ Name ++ "\n"),
  PocketCards = wait_for_pocketcards(),
  io:format("got: ~p~n", [PocketCards]).

connect() ->
  {ok, Socket} = gen_tcp:connect('localhost', 12345, [binary, {packet, 0}]),
  Socket.

wait_for_pocketcards() ->
  receive
    {tcp, _Socket, Message} -> Message
  end.

send(Socket, Message) ->
  gen_tcp:send(Socket, Message).
