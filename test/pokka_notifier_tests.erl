-module(pokka_notifier_tests).
-include_lib("eunit/include/eunit.hrl").
-define(setup(PlayerList, Test), {setup, fun() -> start(PlayerList) end, fun stop/1, fun Test/1}).
-define(join_msg(NewPlayer), {status, "status: new player " ++ atom_to_list(NewPlayer) ++ " joined the table"}).
-define(leave_msg(NewPlayer), {status, "status: player " ++ atom_to_list(NewPlayer) ++ " left the table"}).

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
send_message_on_join_test_() ->
  [
    {"It should send no message if the player list is empty",
    ?setup([], join_no_message_send_on_empty_list)},
    {"It should send a message to the player in the list",
    ?setup([{player1, pid1}], join_send_message_to_player)},
    {"It should send a message to all the players in the list",
    ?setup([{player1, pid1}, {player2, pid2}], join_send_message_to_players)}
  ].

send_message_on_leave_test_() ->
  [
    {"It should send no message if the player list is empty",
    ?setup([], leave_no_message_send_on_empty_list)},
    {"It should send a message to the player in the list",
    ?setup([{player1, pid1}], leave_send_message_to_player)},
    {"It should send a message to all the players in the list",
    ?setup([{player1, pid1}, {player2, pid2}], leave_send_message_to_players)}
  ].

%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%
start(PlayerList) ->
  code:unstick_mod(gen_fsm),
  meck:new(gen_fsm),
  PlayerList.

stop(_PlayerList) ->
  meck:unload(gen_fsm).

%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%

%% Player joins
join_no_message_send_on_empty_list(PlayerList) ->
  meck:expect(gen_fsm, send_event, fun(_Pid, _Message) -> ok end),
  pokka_notifier:join(PlayerList, some_player),
  [
    ?_assert(meck:validate(gen_fsm)),
    ?_assertEqual([], meck:history(gen_fsm))
  ].

join_send_message_to_player(PlayerList = [{_Name, Pid}]) ->
  meck:expect(gen_fsm, send_event, fun(_P, _M) -> ok end),
  NewPlayer = some_new_player,
  pokka_notifier:join(PlayerList, NewPlayer),
  [
    ?_assert(meck:validate(gen_fsm)),
    ?_assert(meck:called(gen_fsm, send_event, [Pid, ?join_msg(NewPlayer)]))
  ].

join_send_message_to_players(PlayerList = [{_Name1, Pid1}, {_Name2, Pid2}]) ->
  meck:expect(gen_fsm, send_event, fun(_P, _M) -> ok end),
  NewPlayer = some_new_player,
  pokka_notifier:join(PlayerList, NewPlayer),
  [
    ?_assert(meck:validate(gen_fsm)),
    ?_assert(meck:called(gen_fsm, send_event, [Pid1, ?join_msg(NewPlayer)])),
    ?_assert(meck:called(gen_fsm, send_event, [Pid2, ?join_msg(NewPlayer)]))
  ].

%% Player leaves
leave_no_message_send_on_empty_list(PlayerList) ->
  meck:expect(gen_fsm, send_event, fun(_Pid, _Message) -> ok end),
  pokka_notifier:leave(PlayerList, some_player),
  [
    ?_assert(meck:validate(gen_fsm)),
    ?_assertEqual([], meck:history(gen_fsm))
  ].

leave_send_message_to_player(PlayerList = [{_Name, Pid}]) ->
  meck:expect(gen_fsm, send_event, fun(_P, _M) -> ok end),
  NewPlayer = some_new_player,
  pokka_notifier:leave(PlayerList, NewPlayer),
  [
    ?_assert(meck:validate(gen_fsm)),
    ?_assert(meck:called(gen_fsm, send_event, [Pid, ?leave_msg(NewPlayer)]))
  ].

leave_send_message_to_players(PlayerList = [{_Name1, Pid1}, {_Name2, Pid2}]) ->
  meck:expect(gen_fsm, send_event, fun(_P, _M) -> ok end),
  NewPlayer = some_new_player,
  pokka_notifier:leave(PlayerList, NewPlayer),
  [
    ?_assert(meck:validate(gen_fsm)),
    ?_assert(meck:called(gen_fsm, send_event, [Pid1, ?leave_msg(NewPlayer)])),
    ?_assert(meck:called(gen_fsm, send_event, [Pid2, ?leave_msg(NewPlayer)]))
  ].
