%% Copyright
-module(pokka_chat_test).
-author("till").

-export([run_test/0]).

run_test() ->
  passed = player_joined_test().

player_joined_test() ->
  ok = application:start(pokka),
  spawn(pokka_test_player, start, [self()]),
  ExpectedMessage = <<"New player 'some body' has joined.\n">>,
  ok = assert_messages_received(ExpectedMessage),
  passed.

%% Helper
assert_messages_received(ExpectedMessage) ->
  receive
    Message -> ExpectedMessage = Message
  end,
  ok.