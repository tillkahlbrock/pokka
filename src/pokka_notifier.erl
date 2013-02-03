-module(pokka_notifier).
-export([join/2, leave/2, pocket_cards/1]).

join([], _NewPlayer) -> ok;

join([{_Player, Pid} | Rest], NewPlayer) ->
  gen_fsm:send_all_state_event(Pid, {status, "status: new player " ++ atom_to_list(NewPlayer) ++ " joined the table"}),
  join(Rest, NewPlayer).

leave([], _NewPlayer) -> ok;

leave([{_Player, Pid} | Rest], NewPlayer) ->
  gen_fsm:send_all_state_event(Pid, {status, "status: player " ++ atom_to_list(NewPlayer) ++ " left the table"}),
  leave(Rest, NewPlayer).

pocket_cards([]) ->
  ok;

pocket_cards([{{_Player, Pid}, _Cards} | Rest]) ->
  gen_fsm:send_all_state_event(Pid, {status, "pocket cards: {kd,p3}"}),
  pocket_cards(Rest).
