-module(player).
-export([join/2, wait_for_pocket_cards/0]).

join(TablePid, Name) ->
  MRef = make_ref(),
  TablePid ! {self(), MRef, {join, Name}},
  receive
    {MRef, ok} -> ok
  after 3000 ->
    exit(timeout_value)
  end.

wait_for_pocket_cards() ->
  receive
    {Table, MRef, {pocket_cards, Cards}} ->
      Table ! {MRef, ok},
      Cards
  after 30000 ->
    exit(timeout_value)
  end.

