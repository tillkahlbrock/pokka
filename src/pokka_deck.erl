-module(pokka_deck).
-export([pocket_cards/1]).

pocket_cards(Players) ->
  PocketCards = lists:foldl(fun(X,Acc) -> Acc++[{X,{kd,p3}}] end, [], Players),
  PocketCards.
