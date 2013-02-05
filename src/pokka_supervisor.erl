-module(pokka_supervisor).
-behaviour(supervisor).
-export([start_link/1]).
-export([init/1]).

start_link(Table) ->
  supervisor:start_link({local,?MODULE}, ?MODULE, [Table]).

init([Table]) ->
  {ok, {
    {one_for_one, 3, 30},
    [
      {
        pokka_table_supervisor,
        {pokka_table_supervisor, start_link, [Table]},
        permanent,
        6000,
        supervisor,
        [pokka_table_supervisor]
      },
      {
        pokka_deck_supervisor,
        {pokka_deck_supervisor, start_link, []},
        permanent,
        6000,
        supervisor,
        [pokka_deck_supervisor]
      },
      {
        pokka_player_supervisor,
        {pokka_player_supervisor, start_link, [Table]},
        permanent,
        6000,
        supervisor,
        [pokka_player_supervisor]
      }
    ]}
  }.
