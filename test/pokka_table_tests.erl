-module(pokka_table_tests).
-include_lib("eunit/include/eunit.hrl").
-record(state, {players=[]}).
-define(setup(InitStateData, Test), {setup, fun() -> start(InitStateData) end, fun stop/1, fun Test/1}).

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
join_new_player_test_() ->
  [{"receiving a join message in idle state, adds the name and the pid "
    "of the new player to the state",
    ?setup(#state{}, first_join_in_idle_state)}
  ].

%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%
start(InitStateData) ->
  pokka_table:start_link(tablename),
  InitStateData.

stop(_InitStateData) ->
  gen_fsm:sync_send_all_state_event(tablename, terminate).

%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%

%% Player joins
first_join_in_idle_state(InitStateData) ->
  Player = {player1, self()},
  {_, _, StateData, _} = pokka_table:idle({join, Player}, InitStateData),
  [?_assertEqual([Player], StateData#state.players)].
