-module(pokka_dispatcher_supervisor).
-behaviour(supervisor).

-export([start_link/0, start_socket/0]).
-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  %{ok, Port} = application:get_env(port),
  Port = 12345,
  {ok, ListenSocket} = gen_tcp:listen(Port, [{active,once}, {packet,line}]),
  spawn_link(fun empty_listeners/0),
  {
    ok,
    {
      {simple_one_for_one, 60, 3600},
      [{
        dispatcher,
        {pokka_dispatcher, start_link, [ListenSocket]},
        temporary, 1000, worker, [pokka_dispatcher]
      }]
    }
  }.

start_socket() ->
  supervisor:start_child(?MODULE, []).

empty_listeners() ->
  [start_socket() || _ <- lists:seq(1,10)],
  ok.
