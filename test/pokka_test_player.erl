-module(pokka_test_player).
-export([start/0]).

start() ->
  receive
    Message -> erlang:display(Message)
  end,
  start().