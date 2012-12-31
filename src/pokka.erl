-module(pokka).
-behaviour(application).
-export([start/2, stop/1, join_table/2, leave_table/2]).

start(normal, _Args) ->
  pokka_supervisor:start_link().

stop(_State) ->
  ok.

join_table(Table, Name) ->
  ok = gen_server:call(Table, {join, Name}),
  ok.

leave_table(Table, Name) ->
  ok = gen_server:call(Table, {leave, Name}),
  ok.
