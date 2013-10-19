-module(pokka_pot).
-define(POT, pokka_pot).
%% API
-export([start/0, stop/0, put/1, size/0, payout/0]).

start() ->
  Pid = spawn_link(fun() -> loop(0) end),
  register(?POT, Pid).

stop() ->
  ?POT ! stop,
  ok.

put(Amount) ->
  ?POT ! {put, Amount},
  ok.

size() ->
  ?POT ! {size, self()},
  receive
    {size, Amount} -> Amount
  end.

payout() ->
  Amount = size(),
  ?POT ! clear,
  Amount.

loop(Size) ->
  receive
    stop -> ok;
    {put, Amount} -> loop(Size + Amount);
    {size, Sender} -> Sender ! Size, loop(Size);
    clear -> loop(0)
  end.