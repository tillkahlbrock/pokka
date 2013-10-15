-module(pokka).
-behaviour(application).
-export([start/2, stop/1, join_table/2, leave_table/2, history/0]).

start(normal, _Args) ->
  pokka_supervisor:start_link(pokka_table).

stop(_State) ->
  ok.

join_table(Table, Player) ->
  gen_fsm:send_event(Table, {join, Player}).

leave_table(Table, Player) ->
  gen_fsm:send_all_state_event(Table, {leave, Player}).

history() ->
  gen_fsm:sync_send_all_state_event(pokka_table, history).