-module(pokka_table).
-behaviour(gen_fsm).
-include("pokka.hrl").

-define(MIN_PLAYERS, 2).
-define(TIMEOUT, 3000).

-export([start_link/2]).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
-export([idle/2, game/2]).

%%%===================================================================
%%% API
%%%===================================================================
start_link(Table, Players) ->
  log("----------------------------"),
  log("Starting new pokka server..."),
  log("----------------------------"),
  gen_fsm:start_link({local, Table}, ?MODULE, #table_state{players = Players}, []).

init(State) ->
  process_flag(trap_exit, true),
  pokka_pot:start(),
  {ok, idle, State}.

%%%===================================================================
%%% state callbacks
%%%===================================================================
idle({join, Player = #player{name=Name}}, StateData = #table_state{players=Players}) ->
  AllPlayers = [Player|Players],
  Info = "New player " ++ Name ++ " has joined",
  ok = send_info(Info, AllPlayers),
  NewStateData = StateData#table_state{players = AllPlayers},
  case length(AllPlayers) of
    Length when Length >= ?MIN_PLAYERS ->
      {next_state, idle, NewStateData, ?TIMEOUT};
    _ ->
      {next_state, idle, NewStateData}
  end;

idle(timeout, StateData = #table_state{players=Players}) ->
  deal_pocket_cards(Players),
  gen_fsm:send_event(self(), pocket_cards_dealed),
  {next_state, game, StateData}.

game(pocket_cards_dealed, StateData = #table_state{players=Players, blinds=Blinds}) ->
  NewBlinds = switch_blinds(Blinds, length(Players)),
  demand_blinds(NewBlinds, Players),
  {next_state, game, StateData#table_state{blinds=NewBlinds}};

game({received_blind, PlayerName, Amount}, StateData = #table_state{players=Players}) ->
  log("Table: Got blind: " ++ Amount ++ " from " ++ PlayerName),
  pokka_pot:put(list_to_integer(Amount)),
  send_info("Pot size: " + integer_to_list(pokka_pot:size()), Players),
  {next_state, game, StateData}.

%%%===================================================================
%%% OTP callbacks
%%%===================================================================
handle_event(_Event, StateName, State) -> {next_state, StateName, State}.

handle_sync_event(history, _From, StateName, State) ->
  {reply, State#table_state.messages, StateName, State};

handle_sync_event(terminate, _From, _StateName, State) -> {stop, normal, ok, State};

handle_sync_event(_Event, _From, StateName, State) -> {reply, unknown, StateName, State}.

handle_info(_Message, StateName, State) -> {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) -> log("Table: Shutting down...").

code_change(_OldVersion, StateName, State, _Extra) -> {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
deal_pocket_cards([]) -> ok;

deal_pocket_cards([Player|Players]) ->
  {ok, {{Card1}, {Card2}}} = gen_server:call(pokka_deck, pocket_cards),
  Message = "POCKETCARDS {" ++ Card1 ++ ", " ++ Card2 ++ "}",
  ok = send_command(Message, Player),
  deal_pocket_cards(Players).

-spec(send_command/2:: (string(), #player{}) -> 'ok').
send_command(Command, #player{pid=Pid, name=Name}) ->
  gen_server:cast(Pid, {command, Command}),
  log("COMMAND -> " ++ Name ++ ": " ++ Command),
  ok.

send_info(_Info, []) -> ok;

send_info(Info, [#player{pid=Pid, name=Name}|Players]) ->
  gen_server:cast(Pid, {info, Info}),
  log("INFO -> " ++ Name ++ ": " ++ Info),
  send_info(Info, Players).

log(Message) ->
  file:write_file(?LOG_FILE, io_lib:fwrite("~p.\n", [Message]), [append]).

switch_blinds(Blinds, PlayersCount) -> [(Blind + 1) rem (PlayersCount + 1) || Blind <- Blinds].

demand_blinds([SBIndex, BBIndex], Players) ->
  SmallBlind = lists:nth(SBIndex, Players),
  BigBlind = lists:nth(BBIndex, Players),
  send_command("BLIND 50", SmallBlind),
  send_command("BLIND 100", BigBlind).
