-module(pokka_chat_test).
-export([run_test/0]).


run_test() ->
  passed = player_joined(),
  passed = second_player_joined().

player_joined() ->
  ok = application:start(pokka),
  spawn(pokka_test_player, start, [("Peter")]),
  timer:sleep(1000),
  [History|_R] = pokka:history(),
  "New player Peter has joined.\n" = History,
  ok = application:stop(pokka),
  passed.

second_player_joined() ->
  ok = application:start(pokka),
  spawn(pokka_test_player, start, [("Bert")]),
  timer:sleep(1000),
  spawn(pokka_test_player, start, [("Ernie")]),
  ExpectedMessage = [
    "New player Bert has joined.\n",
    "New player Ernie has joined.\n",
    "New player Ernie has joined.\n"
  ],
  timer:sleep(1000),
  ExpectedMessage = pokka:history(),
  ok = application:stop(pokka),
  passed.