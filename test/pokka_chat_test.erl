-module(pokka_chat_test).
-export([run_test/0]).


run_test() ->
  %%passed = player_joined(),
  %%passed = second_player_joined(),
  passed = deal_cards_when_two_players_joined().

player_joined() ->
  ok = application:start(pokka),
  spawn(pokka_test_player, join, [("Peter")]),
  timer:sleep(1000),
  ["New player Peter has joined.\n"] = pokka:history(),
  ok = application:stop(pokka),
  passed.

second_player_joined() ->
  ok = application:start(pokka),
  spawn(pokka_test_player, join, [("Bert")]),
  timer:sleep(1000),
  spawn(pokka_test_player, join, [("Ernie")]),
  timer:sleep(1000),
  [
    "New player Bert has joined.\n",
    "New player Ernie has joined.\n"
  | _Rest] = pokka:history(),
  ok = application:stop(pokka),
  passed.

deal_cards_when_two_players_joined() ->
  ok = application:start(pokka),
  spawn(pokka_test_player, join, [("Bert")]),
  timer:sleep(1000),
  spawn(pokka_test_player, join, [("Ernie")]),
  timer:sleep(1000),
  [
    "New player Bert has joined.\n",
    "New player Ernie has joined.\n",
    "CARDS Bert " ++ _Cards1,
    "CARDS Ernie " ++ _Cards2
  | _Rest] = pokka:history(),
  ok = application:stop(pokka),
  passed.