-module(pokka_app).
-export([start/0, start_player/1, stop/0, stop_player/1, loop/0]).

start() ->
  Pid = spawn(?MODULE, loop, []),
  register(pokka_app, Pid),
  pokka_app ! start.

start_player(Name) ->
  Pid = spawn(pokka_test_player, start_link, [atom_to_list(Name)]),
  register(Name, Pid).

stop_player(Name) ->
  Name ! stop.

stop() ->
  pokka_app ! stop.

loop() ->
  receive
    stop -> application:stop(pokka);
    start ->
      application:start(pokka),
      loop()
  end.