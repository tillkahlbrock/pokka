-module(pokka_player_supervisor).
-behaviour(supervisor).

-export([start_link/1, start_socket/0]).
-export([init/1]).

start_link(Table) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, [Table]).

init([Table]) ->
  %{ok, Port} = application:get_env(port),
  Port = 12345,
  {ok, ListenSocket} = gen_tcp:listen(Port, [{active,once}, {packet,line}, {reuseaddr, true}]),
  spawn_link(fun empty_listeners/0),
  {
    ok,
    {
      {simple_one_for_one, 60, 3600},
      [{
        player,
        {pokka_player, start_link, [ListenSocket, Table]},
        temporary, 1000, worker, [pokka_player]
      }]
    }
  }.

start_socket() ->
  supervisor:start_child(?MODULE, []).

empty_listeners() ->
  [start_socket() || _ <- lists:seq(1,10)],
  ok.
