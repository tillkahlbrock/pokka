-module(pokka_test_player).

%% API
-export([start/1, start/2]).

start(PlayerName) ->
  {ok, Socket} = gen_tcp:connect('localhost', 12345, [binary, {packet, 0}]),
  Message = "JOIN " ++ PlayerName ++ "\n",
  ok = gen_tcp:send(Socket, Message),
  ok.

start(TestCase, PlayerName) ->
  ok = start(PlayerName),
  run(TestCase, []).

run(TestCase, Messages) ->
  receive
    {tcp, _Socket, Message} ->
      run(TestCase, Messages ++ Message)
  after 2000 -> TestCase ! Messages
  end.