-module(pokka_test_player).

%% API
-export([start/1]).

start(TestCase) ->
  {_Status, _Socket} = gen_tcp:connect('localhost', 12345, [binary, {packet, 0}]),
  run(TestCase, []).

run(TestCase, Messages) ->
  receive
    {tcp, _Socket, M1} ->
      run(TestCase, Messages ++ M1)
  after 2000 -> TestCase ! Messages
  end.