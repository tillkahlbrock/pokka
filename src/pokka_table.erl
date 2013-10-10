-module(pokka_table).
-behaviour(gen_fsm).
-export([start_link/2]).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
-export([idle/2]).
-record(state, {players = [], messages = []}).

start_link(Table, Players) -> gen_fsm:start_link({local, Table}, ?MODULE, #state{players = Players}, []).

init(State) -> {ok, idle, State}.

idle({join, Player = {Name, _Pid}}, State) ->
  AllPlayers = [Player|State#state.players],
  Message = "New player " ++ Name ++ " has joined.\n",
  ok = sendToPlayers(AllPlayers, Message),
  NewState = State#state{players = AllPlayers, messages = State#state.messages ++ [Message]},
  case length(AllPlayers) of
    Length when Length >= 2 -> {next_state, idle, NewState, 3000};
    _ -> {next_state, idle, NewState}
  end;

idle(timeout, StateData) ->
  AllPlayers = StateData#state.players,
  Messages = deal_pocket_cards(AllPlayers),
  NewStateData = StateData#state{players = AllPlayers, messages = StateData#state.messages ++ [Messages]},
  {next_state, game, NewStateData}.

handle_event(_Event, StateName, State) -> {next_state, StateName, State}.

handle_sync_event(history, _From, StateName, State) ->
  {reply, State#state.messages, StateName, State};

handle_sync_event(terminate, _From, _StateName, State) -> {stop, normal, ok, State};

handle_sync_event(_Event, _From, StateName, State) -> {reply, unknown, StateName, State}.

handle_info(_Message, StateName, State) -> {next_state, StateName, State}.

terminate(_Reason, _StateName, State) -> io:format("shutting down. state: ~p~n", [State]).

code_change(_OldVersion, StateName, State, _Extra) -> {ok, StateName, State}.

deal_pocket_cards(Players) ->
  PlayerMessages = [{Player, "CARDS " ++ Name ++ "{CC,BB}\n"} || Player = {Name, _Pid} <- Players],
  lists:foreach(fun({Player, Message}) -> sendToPlayer(Player, Message) end, PlayerMessages),
  lists:map(fun({_Player, Message}) -> Message end, PlayerMessages).

sendToPlayers([], _Message) -> ok;

sendToPlayers([Player|Rest], Message) ->
  sendToPlayer(Player, Message),
  sendToPlayers(Rest, Message).

sendToPlayer({_Name, Pid}, Message) ->
  gen_fsm:send_all_state_event(Pid, {message, Message}).