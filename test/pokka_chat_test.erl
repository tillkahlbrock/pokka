%% Copyright
-module(pokka_chat_test).
-author("till").

-export([run_test/0]).

run_test() ->
  passed = player_joined_test().

player_joined_test() ->
  ok = application:start(pokka),
  PlayerName = "a player name",
  ExpectedMessage = <<"New player '\"a_player_name\"' has joined.\n">>,
  spawn(pokka_test_player, start, [self(), PlayerName]),
  ok = assert_messages_received(ExpectedMessage),
  passed.

%% Helper
assert_messages_received(ExpectedMessage) ->
  receive
    Message -> ExpectedMessage = Message
  end,
  ok.