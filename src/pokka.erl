-module(pokka).
-behaviour(application).
-export([start/2, stop/1, add_player/1, kill_player/1]).

start(normal, _Args) ->
  pokka_supervisor:start_link().

stop(_State) ->
  ok.

add_player(Name) ->
  pokka_player_supervisor:start_player(Name).

kill_player(Name) ->
  pokka_player_supervisor:kill_player(Name).
