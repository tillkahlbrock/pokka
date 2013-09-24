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
  application:start(pokka),
  timer:sleep(3000).

stop(_InitData) ->
  application:stop(pokka).

%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%
receive_welcome_message(_InitData) ->
  {_Socket, Helo} = connect(),
  [
    ?_assertEqual(<<"Hello you! Wanna play some poker?\n">>, Helo)
  ].

%%%%%%%%%%%%%%%%%%%
%%% TEST HELPER %%%
%%%%%%%%%%%%%%%%%%%
connect() ->
  Socket = con(10),
  Helo = receive
    {tcp,_Socket,M1} -> M1
    after 2000 -> "received time while waiting for greeting."
  end,
  {Socket, Helo}.

con(0) -> exit('cant connect to server');

con(Tries) ->
  {Status, Socket} = gen_tcp:connect('localhost', 12345, [binary, {packet, 0}]),
  if
    Status == ok -> Socket;
    true -> timer:sleep(1000), con(Tries-1)
  end.
