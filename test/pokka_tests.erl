-module(pokka_tests).
-include_lib("eunit/include/eunit.hrl").
-define(setup(Test), {setup, fun() -> start([]) end, fun stop/1, fun Test/1}).

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
integration_test_() ->
  [
    ?setup(receive_welcome_message)
  ].

%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%
start(_InitData) ->
  application:start(pokka).

stop(_InitData) ->
  application:stop(pokka).

%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%
receive_welcome_message(_InitData) ->
  Host = 'localhost',
  Port = 12345,
  {ok, Sock} = gen_tcp:connect(Host, Port, [binary, {packet, 0}]),
  Message = receive
    {tcp,Sock,Helo} -> Helo
    after 2000 -> "received time while waiting for greeting."
  end,
  [
    ?_assertEqual(<<"Hello you! Wanna play some poker?\n">>, Message)
  ].
