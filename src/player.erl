-module(player).
-export([join/2]).

join(TablePid, Name) ->
  MRef = make_ref(),
  TablePid ! {self(), MRef, {join, Name}},
  receive
    {MRef, ok} -> ok
  after 2000 ->
    ptimeout
  end.
  
