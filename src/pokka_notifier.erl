-module(pokka_notifier).
-export([join/2, leave/2, deal_pocket_cards/1]).

join([], _NewPlayer) -> ok;

join([{_Player, Pid} | Rest], NewPlayer) ->
  gen_fsm:send_all_state_event(Pid, {status, "status: new player " ++ atom_to_list(NewPlayer) ++ " joined the table"}),
  join(Rest, NewPlayer).

leave([], _NewPlayer) -> ok;

leave([{_Player, Pid} | Rest], NewPlayer) ->
  gen_fsm:send_all_state_event(Pid, {status, "status: player " ++ atom_to_list(NewPlayer) ++ " left the table"}),
  leave(Rest, NewPlayer).

deal_pocket_cards(Players) ->
  lists:map(
    fun({_Player, Pid}) ->
      Cards = pokka_deck:pocket_cards(),
      gen_fsm:send_all_state_event(Pid, {status, "pocket cards: dudidum"})
    end,
    Players
  ),
  ok.
