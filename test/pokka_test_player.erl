-module(pokka_test_player).

%% API
-export([join/1]).

join(PlayerName) ->
  Socket = connect(),
  Message = "JOIN " ++ PlayerName ++ "\n",
  ok = gen_tcp:send(Socket, Message),
  ok.

connect() ->
  {ok, Socket} = gen_tcp:connect('localhost', 12345, [binary, {packet, 0}]),
  Socket.