%% Copyright
-module(pokka_chat_test).
-author("till").

-export([run_test/0]).

run_test() ->
  passed = player_joined_test().

player_joined_test() ->
  ok = application:start(pokka),
  <<"New player 'some body' has joined.\n">> = pokka_test_player:start(),
  passed.
