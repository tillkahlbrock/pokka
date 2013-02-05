-module(pokka_deck_supervisor).
-behavior(supervisor).
-export([start_link/0]).
-export([init/1]).


start_link() ->
  supervisor:start_link({local,?MODULE}, ?MODULE, []).

init([]) ->
  {ok, {
    {one_for_one, 5, 60},
    [
      {pokka_deck,
      {pokka_deck, start_link, []},
      permanent,
      5000,
      worker,
      [pokka_deck]}
    ]}
  }.
