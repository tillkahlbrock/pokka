-module(table).
-export([init/0]).
-record(state, {players=[]}).
-record(player, {
  name,
  pid
  }).

init() ->
  timer:send_after(20000, {join_over}),
  join_loop(#state{players=[]}).

join_loop(State) ->
  receive
    {Pid, MRef, {join, Name}} ->
      Players = State#state.players,
      NewState = State#state{players=[#player{name=Name, pid=Pid}|Players]},
      Pid ! {MRef, ok},
      join_loop(NewState);
    {join_over} ->
      play_loop(State);
    {kill} ->
      exit(normal)
  end.

play_loop(State) ->
  send_cards(State#state.players).
  
send_cards([]) -> true; % Jump to betting loop...
send_cards([Player|Rest]) ->
  Player#player.pid ! {pocket_cards, {dh,kk}},
  send_cards(Rest).
