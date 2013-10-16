-module(pokka_app).
-export([start/0, start_player/1, stop/0]).

start() ->
  Pid = spawn(fun() -> loop() end),
  register(pokka_app, Pid),
  pokka_app ! start.

stop() ->
  pokka_app ! stop.

start_player(Name) ->
  spawn(pokka_test_player, start_link, [Name]).


loop() ->
  receive
    stop -> application:stop(pokka);
    start ->
      application:start(pokka),
      loop()
  end.