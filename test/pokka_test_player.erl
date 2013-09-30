-module(pokka_test_player).

%% API
-export([start/0]).

start() ->
  {_Status, _Socket} = gen_tcp:connect('localhost', 12345, [binary, {packet, 0}]),
  receive
    {tcp, _Socket, M1} -> M1
  after 2000 -> "received timeout while waiting for greeting."
  end.