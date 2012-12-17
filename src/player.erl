-module(player).
-export([join/2, bet/1]).

join(TablePid, Name) ->
  MRef = make_ref(),
  TablePid ! {self(), MRef, {join, Name}},
  receive
    {MRef, ok} -> ok,
    PocketCards = receive_pocket_cards(),
    PocketCards
  after 2000 ->
    ptimeout
  end.
  
receive_pocket_cards() ->
  receive
    {pocket_cards, PocketCards} ->
      PocketCards
  end.

bet(Cards) ->
  io:format("got cards: ~p~n", [Cards]).
