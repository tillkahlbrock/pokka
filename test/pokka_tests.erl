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
  {Socket1, Helo} = connect(),
  GambleMsg = send_join(Socket1, 'till'),
  {Socket2, _Helo} = connect(),
  _GambleMsg = send_join(Socket2, 'klaus'),
  StatusJoin = receive_status(),
  send_leave(Socket1), %% till leaves
  StatusLeave = receive_status(),
  PocketCards = receive_pc(),
  [
    ?_assertEqual(<<"Hello you! Wanna play some poker?\n">>, Helo),
    ?_assertEqual(<<"Ok till, lets gamble!\n">>, GambleMsg),
    ?_assertEqual(<<"status: new player klaus joined the table\n">>, StatusJoin),
    ?_assertEqual(<<"status: player till left the table\n">>, StatusLeave),
    ?_assertMatch("pocket cards: "++_, binary_to_list(PocketCards))
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
    after 2000 -> "received time while waiting for join ack."
  end,
  GambleMsg.

send_leave(Socket) ->
  gen_tcp:send(Socket, io_lib:format("quit~n", [])).

receive_status() ->
  receive
    {tcp,_Socket,Msg} -> Msg
    after 2000 -> "received timeout while waiting for status message."
  end.

receive_pc() ->
  receive
    {tcp,_Socket,Msg} -> Msg
    after 6000 -> "received timeout while waiting for pocket cards."
  end.
