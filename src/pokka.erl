-module(pokka).
-export([start_link/0, stop/0, add_player/1, kill_player/1]).

start_link() ->
  pokka_supervisor:start_link().

stop() ->
  pokka_supervisor:stop().

add_player(Name) ->
  pokka_player_supervisor:start_player(Name).

kill_player(Name) ->
  pokka_player_supervisor:kill_player(Name).
