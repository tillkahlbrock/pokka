-module(pokka_chat_test).
-export([run_test/0]).


run_test() ->
  passed = player_joined().
  %%passed = second_player_joined().

player_joined() ->
  ok = application:start(pokka),
  PlayerName = "Peter",
  spawn(pokka_test_player, start, [PlayerName]),
  timer:sleep(1000),
  [History|_R] = pokka:history(),
  "New player Peter has joined.\n" = History,
  ok = application:stop(pokka),
  passed.

second_player_joined() ->
  ok = application:start(pokka),
  PlayerName1 = "a player name",
  PlayerName2 = "another player name",
  ExpectedMessage = <<"New player '\"a_player_name\"' has joined.\nNew player '\"another_player_name\"' has joined.\n">>,
  spawn(pokka_test_player, start, [self(), PlayerName1]),
  spawn(pokka_test_player, start, [PlayerName2]),
  ok = assert_messages_received(ExpectedMessage),
  ok = application:stop(pokka),
  passed.

%% Helper
assert_messages_received(ExpectedMessage) ->
  receive
    Message -> ExpectedMessage = Message
  end,
  ok.