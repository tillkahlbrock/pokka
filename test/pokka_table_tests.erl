-module(pokka_table_tests).
-include_lib("eunit/include/eunit.hrl").
-record(state, {players=[]}).
-define(setup(Test), {setup, fun() -> start(#state{}) end, fun stop/1, fun Test/1}).
-define(setup(InitStateData, Test), {setup, fun() -> start(InitStateData) end, fun stop/1, fun Test/1}).
-define(SOME_PID, list_to_pid("<0.77.0>")).

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
join_new_player_test_() ->
  [
    {"it should add a player to the empty players list",
    ?setup(first_join_in_idle_state)},
    {"it should add a second player to the players list and keep the existing one",
    ?setup(#state{players=[{player1, ?SOME_PID}]}, second_join_in_idle_state)},
    {"it should send a notification to the player when a new player joins",
    ?setup(#state{players=[{player1, ?SOME_PID}]}, send_notification_to_players_on_join)},
    {"it should change the state to game on timeout in idle state",
    ?setup(change_to_game_on_timeout_in_idle_state)},
    {"it should remove the player from player list on leave",
    ?setup(#state{players=[{player1, ?SOME_PID}, {player2, ?SOME_PID}]}, remove_player_on_leave)},
    {"it should send a notification to the player when a player leaves",
    ?setup(#state{players=[{player1, ?SOME_PID}, {player2, ?SOME_PID}]}, send_notification_to_players_on_leave)},
    {"it should stop after receiving a terminate message",
    ?setup(stop_after_terminate_message)}
  ].

%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%
start(InitStateData) ->
  meck:new(pokka_notifier),
  meck:expect(pokka_notifier, join, fun(_PL, _NP) -> ok end),
  meck:expect(pokka_notifier, leave, fun(_PL, _NP) -> ok end),
  InitStateData.

stop(_InitStateData) ->
  meck:unload(pokka_notifier).

%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%

%% Player joins
first_join_in_idle_state(InitStateData) ->
  Player = {player1, ?SOME_PID},
  {_, _, StateData, _} = pokka_table:idle({join, Player}, InitStateData),
  [?_assertEqual([Player], StateData#state.players)].

second_join_in_idle_state(InitStateData = #state{players=[Player]}) ->
  {_, _, StateData, _} = pokka_table:idle({join, Player}, InitStateData),
  [?_assertEqual([Player, {player1, ?SOME_PID}], StateData#state.players)].

send_notification_to_players_on_join(InitStateData = #state{players=PlayerList}) ->
  NewPlayer = player2,
  Player = {NewPlayer, ?SOME_PID},
  pokka_table:idle({join, Player}, InitStateData),
  [
    ?_assert(meck:validate(pokka_notifier)),
    ?_assert(meck:called(pokka_notifier, join, [PlayerList, NewPlayer]))
  ].

change_to_game_on_timeout_in_idle_state(InitStateData) ->
  meck:expect(pokka_notifier, deal_pocket_cards, fun(_Players) -> ok end),
  Result = pokka_table:idle(timeout, InitStateData),
  [?_assertEqual({next_state, game, InitStateData}, Result)].

remove_player_on_leave(InitStateData = #state{players=[Player1, Player2]}) ->
  Result = pokka_table:handle_event({leave, Player1}, some_state, InitStateData),
  [?_assertEqual({next_state, some_state, #state{players=[Player2]}, 5000}, Result)].

send_notification_to_players_on_leave(InitStateData = #state{players=[Player1, Player2 = {LeavingPlayer, _Pid}]}) ->
  pokka_table:handle_event({leave, Player2}, some_state, InitStateData),
  [
    ?_assert(meck:validate(pokka_notifier)),
    ?_assert(meck:called(pokka_notifier, leave, [[Player1], LeavingPlayer]))
  ].

stop_after_terminate_message(InitStateData) ->
  Result = pokka_table:handle_sync_event(terminate, sender, some_state, InitStateData),
  [?_assertEqual({stop, normal, ok, InitStateData}, Result)].
