-module(pokka_player).
-behavior(gen_server).
-export([leave_table/1, start/2, start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

leave_table(Name) -> gen_server:call(Name, terminate).

start(Name, Table) -> gen_server:start({local, Name}, ?MODULE, [Name, Table], []).

start_link(Name, Table) -> gen_server:start_link({local, Name}, ?MODULE, [Name, Table], []).

init(State = [Name, Table]) ->
  gen_server:call(Table, {join, Name}),
  {ok, State}.

handle_call(terminate, _From, State) -> {stop, normal, ok, State}.

handle_cast(_Message, State) -> {noreply, State}.

handle_info(_Message, State) -> {noreply, State}.

terminate(normal, [Name, Table]) ->
  io:format("player ~p leaving table ~p~n", [Name, Table]).

code_change(_OldVersion, State, _Extra) -> {ok, State}.
