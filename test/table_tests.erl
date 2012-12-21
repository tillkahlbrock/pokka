-module(table_tests).
-include_lib("eunit/include/eunit.hrl").
-record(state, {players=[]}).
-record(player, {name, pid}).

handle_call_join_should_add_the_player_to_the_empty_players_list_test_() ->
  Name = some_name,
  From = some_pid,
  InitialState = #state{players=[]},
  NewState = #state{players=[#player{name=Name,pid=From}]},
  [?_assertEqual({reply, ok, NewState}, table:handle_call({join, Name}, From, InitialState))].

handle_call_terminate_should_return_a_stop_normal_signal_test_() ->
  [?_assertEqual({stop, normal, ok, state}, table:handle_call(terminate, from, state))].

handle_cast_should_ignore_any_casts_test_() ->
  [?_assertEqual({noreply, state},  table:handle_cast(message, state))].

handle_info_should_ignore_all_unknown_messages_test_() ->
  [?_assertEqual({noreply, state},  table:handle_info(message, state))].

code_change_should_do_nothing_test_() ->
  [?_assertEqual({ok, state},  table:code_change(oldversion, state, extra))].
