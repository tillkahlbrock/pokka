-module(pokka_notifier).
-export([join/2]).

join([], _NewPlayer) -> ok;

join([{_Player, Pid} | Rest], NewPlayer) ->
  gen_fsm:send_event(Pid, {status, "status: new player " ++ atom_to_list(NewPlayer) ++ " joined the table"}),
  join(Rest, NewPlayer).
