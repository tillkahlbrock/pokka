-module(pokka_table_tests).
-include_lib("eunit/include/eunit.hrl").
-record(state, {players=[]}).
-define(setup(InitStateData, Test), {setup, fun() -> start(InitStateData) end, fun stop/1, fun Test/1}).
-define(SOME_PID, list_to_pid("<0.77.0>")).

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
join_new_player_test_() ->
  [{"receiving a join message in idle state, adds the name and the pid "
    "of the new player to the state",
    ?setup(#state{}, first_join_in_idle_state)},
    {"second player joins in idle state leads to player list with two players",
    ?setup(#state{players=[{player1, ?SOME_PID}]}, second_join_in_idle_state)},
    {"it should send a notification to the player when a new player joins",
    ?setup(#state{players=[{player1, ?SOME_PID}]}, send_notification_to_players_on_join)}
  ].

%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%
start(InitStateData) ->
  code:unstick_mod(gen_fsm),
  meck:new(gen_fsm, [passthrough]),
  InitStateData.

stop(_InitStateData) ->
  meck:unload(gen_fsm).

%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%

%% Player joins
first_join_in_idle_state(InitStateData) ->
  Player = {player1, ?SOME_PID},
  {_, _, StateData, _} = pokka_table:idle({join, Player}, InitStateData),
  [?_assertEqual([Player], StateData#state.players)].

second_join_in_idle_state(InitStateData) ->
  Player = {player2, ?SOME_PID},
  {_, _, StateData, _} = pokka_table:idle({join, Player}, InitStateData),
  [?_assertEqual([Player, {player1, ?SOME_PID}], StateData#state.players)].

send_notification_to_players_on_join(InitStateData) ->
  meck:expect(gen_fsm, send_all_state_event, fun(Pid, Message) -> {ok, Pid, Message} end),
  Player = {player2, ?SOME_PID},
  pokka_table:idle({join, Player}, InitStateData),
  [?_assert(meck:validate(gen_fsm))].
