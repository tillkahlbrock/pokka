-module(pokka).
-behaviour(application).
-export([start/2, stop/1, join_table/3, leave_table/3]).

start(normal, _Args) ->
  pokka_supervisor:start_link(pokka_table).

stop(_State) ->
  ok.

join_table(Table, Name, PlayerPid) ->
  gen_fsm:send_event(Table, {join, Name, PlayerPid}).

leave_table(Table, Name, PlayerPid) ->
  gen_fsm:send_all_state_event(Table, {leave, Name, PlayerPid}).
