-module(player).
-export([join/2, close_table/1]).

join(TablePid, Name) -> gen_server:call(TablePid, {join, Name}).

close_table(TablePid) -> gen_server:call(TablePid, terminate).
