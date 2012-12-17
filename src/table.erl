-module(table).
-export([init/0]).
-record(state, {players=[]}).
-record(player, {
  name,
  pid
  }).

init() ->
  loop(#state{players=[]}).

loop(State) ->
  receive
    {Pid, MRef, {join, Name}} ->
      Players = State#state.players,
      NewState = State#state{players=[#player{name=Name, pid=Pid}|Players]},
      Pid ! {MRef, ok},
      loop(NewState);
    {kill} ->
      exit(normal)
  end.
