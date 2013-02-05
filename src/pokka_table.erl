-module(pokka_table).
-behaviour(gen_fsm).
-export([start_link/1]).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
-export([idle/2]).
-record(state, {players=[]}).

start_link(Table) -> gen_fsm:start_link({local,Table}, ?MODULE, #state{}, []).

init(State) -> {ok, idle, State}.

idle({join, Player = {Name, _Pid}}, State) ->
  Players = State#state.players,
  NewState = State#state{players=[Player|Players]},
  pokka_notifier:join(Players, Name),
  {next_state, idle, NewState, 5000};

idle(timeout, StateData) ->
  PocketCards = pokka_deck:pocket_cards(StateData#state.players),
  pokka_notifier:pocket_cards(PocketCards),
  {next_state, game, StateData}.

handle_event({leave, Player = {Name, _Pid}}, StateName, StateData) ->
  Players = lists:delete(Player, StateData#state.players),
  NewStateData = StateData#state{players=Players},
  pokka_notifier:leave(Players, Name),
  {next_state, StateName, NewStateData, 5000};

handle_event(_Event, StateName, State) -> {next_state, StateName, State}.

handle_sync_event(terminate, _From, _StateName, State) -> {stop, normal, ok, State};

handle_sync_event(_Event, _From, StateName, State) -> {reply, unknown, StateName, State}.

handle_info(_Message, StateName, State) -> {next_state, StateName, State}.

terminate(normal, _StateName, State) -> io:format("shutting down. state: ~p~n", [State]).

code_change(_OldVersion, StateName, State, _Extra) -> {ok, StateName, State}.
