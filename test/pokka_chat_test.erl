%% Copyright
-module(pokka_chat_test).
-author("till").

-export([simple_test/0]).

simple_test() ->
  ok = application:start(pokka),
  <<"New player 'some body' has joined.\n">> = pokka_test_player:start().
