-module(pokka_table).
-behaviour(gen_fsm).
-include("pokka.hrl").

-define(MIN_PLAYERS, 2).
-define(TIMEOUT, 3000).

-export([start_link/2]).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
-export([idle/2]).

start_link(Table, Players) -> gen_fsm:start_link({local, Table}, ?MODULE, #table_state{players = Players}, []).

init(State) -> {ok, idle, State}.

idle({join, Player = #player{name=Name}}, StateData = #table_state{players=Players}) ->
  AllPlayers = [Player|Players],
  Info = "New player " ++ Name ++ " has joined.\n",
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
  {next_state, game, StateData}.

handle_event(_Event, StateName, State) -> {next_state, StateName, State}.

handle_sync_event(history, _From, StateName, State) ->
  {reply, State#table_state.messages, StateName, State};

handle_sync_event(terminate, _From, _StateName, State) -> {stop, normal, ok, State};

handle_sync_event(_Event, _From, StateName, State) -> {reply, unknown, StateName, State}.

handle_info(_Message, StateName, State) -> {next_state, StateName, State}.

terminate(_Reason, _StateName, State) -> io:format("shutting down. state: ~p~n", [State]).

code_change(_OldVersion, StateName, State, _Extra) -> {ok, StateName, State}.

deal_pocket_cards([]) -> ok;

deal_pocket_cards([Player|Players]) ->
  {ok, {{Card1}, {Card2}}} = gen_server:call(pokka_deck, pocket_cards),
  Message = "POCKETCARDS {" ++ Card1 ++ ", " ++ Card2 ++ "}",
  ok = send_command(Message, Player),
  deal_pocket_cards(Players).

send_command(Command, #player{pid=Pid}) ->
  gen_server:cast(Pid, {command, Command}).

send_info(_Info, []) -> ok;

send_info(Info, [#player{pid=Pid}|Players]) ->
  gen_server:cast(Pid, {info, Info}),
  send_info(Info, Players).
