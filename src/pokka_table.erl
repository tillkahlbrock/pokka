-module(pokka_table).
-behaviour(gen_server).
-export([start/0, start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {players=[]}).
-record(player, {name, pid}).

start() -> gen_server:start(?MODULE, #state{}, []).

start_link(Table) -> gen_server:start_link({local,Table}, ?MODULE, #state{}, []).

init(State) -> {ok, State}.

handle_call({join, Name, Pid}, _From, State) ->
  Players = State#state.players,
  Player = #player{name=Name, pid=Pid},
  NewState = State#state{players=[Player|Players]},
  {reply, ok, NewState};

handle_call({leave, Name}, From, State) ->
  Player = #player{name=Name, pid=From},
  Players = lists:delete(Player, State#state.players),
  NewState = State#state{players=Players},
  {reply, ok, NewState};

handle_call(terminate, _From, State) -> {stop, normal, ok, State}.

handle_cast(_Message, State) -> {noreply, State}.

handle_info(_Message, State) -> {noreply, State}.

terminate(normal, State) -> io:format("shutting down. state: ~p~n", [State]).

code_change(_OldVersion, State, _Extra) -> {ok, State}.
