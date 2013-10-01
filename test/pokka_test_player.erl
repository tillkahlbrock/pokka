-module(pokka_test_player).

%% API
-export([start/2]).

start(TestCase, PlayerName) ->
  {ok, Socket} = gen_tcp:connect('localhost', 12345, [binary, {packet, 0}]),
  timer:sleep(1000),
  Message = io_lib:format("JOIN ~p~n", [PlayerName]),
  ok = gen_tcp:send(Socket, Message),
  run(TestCase, []).

run(TestCase, Messages) ->
  receive
    {tcp, _Socket, Message} ->
      run(TestCase, Messages ++ Message)
  after 2000 -> TestCase ! Messages
  end.