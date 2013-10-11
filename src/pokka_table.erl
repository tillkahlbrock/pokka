-module(pokka_table).
-behaviour(gen_fsm).
-include("pokka.hrl").

-export([start_link/2]).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
-export([idle/2]).

start_link(Table, Players) -> gen_fsm:start_link({local, Table}, ?MODULE, #table_state{players = Players}, []).

init(State) -> {ok, idle, State}.

idle({join, Player = #player{name=Name}}, State) ->
  AllPlayers = [Player|State#table_state.players],
  Info = "New player " ++ Name ++ " has joined.\n",
  ok = sendInfo(Info, AllPlayers),
  NewState = State#table_state{players = AllPlayers},
  case length(AllPlayers) of
    Length when Length >= 2 -> {next_state, idle, NewState, 3000};
    _ -> {next_state, idle, NewState}
  end;

idle(timeout, StateData) ->
  AllPlayers = StateData#table_state.players,
  Messages = deal_pocket_cards(AllPlayers),
  NewStateData = StateData#table_state{players = AllPlayers, messages = StateData#table_state.messages ++ [Messages]},
  {next_state, game, NewStateData}.

handle_event(_Event, StateName, State) -> {next_state, StateName, State}.

handle_sync_event(history, _From, StateName, State) ->
  {reply, State#table_state.messages, StateName, State};

handle_sync_event(terminate, _From, _StateName, State) -> {stop, normal, ok, State};

handle_sync_event(_Event, _From, StateName, State) -> {reply, unknown, StateName, State}.

handle_info(_Message, StateName, State) -> {next_state, StateName, State}.

terminate(_Reason, _StateName, State) -> io:format("shutting down. state: ~p~n", [State]).

code_change(_OldVersion, StateName, State, _Extra) -> {ok, StateName, State}.

deal_pocket_cards(Players) ->
  PlayerMessages = [{Player, "CARDS " ++ Name ++ "{CC,BB}\n"} || Player = {Name, _Pid} <- Players],
  lists:foreach(fun({Player, Message}) -> sendToPlayer(Player, Message) end, PlayerMessages),
  lists:map(fun({_Player, Message}) -> Message end, PlayerMessages).

sendToPlayer({_Name, Pid}, Message) ->
  gen_fsm:send_all_state_event(Pid, {message, Message}).

sendCommand(Command, Recipient) ->
  Recipient ! Command.

sendInfo(_Info, []) -> ok;

sendInfo(Info, [#player{pid=Pid}|Players]) ->
  gen_server:cast(Pid, {info, Info}),
  sendInfo(Info, Players).
