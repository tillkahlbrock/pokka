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
  Player = 'till',
  {Socket, Helo} = connect(),
  GambleMsg = send_join(Socket, Player),
  [
    ?_assertEqual(<<"Hello you! Wanna play some poker?\n">>, Helo),
    ?_assertEqual(<<"Ok till, lets gamble!\n">>, GambleMsg)
  ].

%%%%%%%%%%%%%%%%%%%
%%% TEST HELPER %%%
%%%%%%%%%%%%%%%%%%%
connect() ->
  {ok, Socket} = gen_tcp:connect('localhost', 12345, [binary, {packet, 0}]),
  Helo = receive
    {tcp,_Socket,M1} -> M1
    after 2000 -> "received time while waiting for greeting."
  end,
  {Socket, Helo}.

send_join(Socket, Player) ->
  gen_tcp:send(Socket, io_lib:format("join ~p~n", [Player])),
  GambleMsg = receive
    {tcp,_Socket,M2} -> M2
    after 2000 -> "received time while waiting for greeting."
  end,
  GambleMsg.
