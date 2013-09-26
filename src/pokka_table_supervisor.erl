-module(pokka_table_supervisor).
-behavior(supervisor).
-export([start_link/1]).
-export([init/1]).


start_link(Table) ->
  supervisor:start_link({local,?MODULE}, ?MODULE, [Table, []]).

init(TableInitState) ->
  {ok, {
    {one_for_one, 5, 60},
    [
      {pokka_table,
      {pokka_table, start_link, TableInitState},
      permanent,
      5000,
      worker,
      [pokka_table]}
    ]}
  }.
