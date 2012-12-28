-module(pokka_player_supervisor).
-behaviour(supervisor).
-export([start_link/0, start_player/1, stop_player/1]).
-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  MaxRestart = 6,
  MaxTime = 3600,
  {ok, {{one_for_one, MaxRestart, MaxTime}, []}}.

start_player(Name) ->
  ChildSpec = {
    Name,
    {pokka_player, start_link, [Name, pokka_table]},
    temporary, 2000, worker, [pokka_player]
  },
  supervisor:start_child(?MODULE, ChildSpec).

stop_player(Name) ->
  supervisor:terminate_child(?MODULE, Name),
  supervisor:delete_child(?MODULE, Name).
