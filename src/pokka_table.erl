-module(pokka_table).
-behaviour(gen_fsm).
-export([start_link/2]).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
-export([idle/2]).
-record(state, {players=[], messages=[]}).

start_link(Table, Players) -> gen_fsm:start_link({local,Table}, ?MODULE, #state{players=Players}, []).

init(State) -> {ok, idle, State}.

idle({join, Player = {Name, _Pid}}, State) ->
  AllPlayers = [Player|State#state.players],
  Message = "New player " ++ Name ++ " has joined.\n",
  MessagesSend = send(AllPlayers, Message),
  {next_state, idle, State#state{players=AllPlayers, messages=State#state.messages ++ MessagesSend}}.

handle_event(_Event, StateName, State) -> {next_state, StateName, State}.

handle_sync_event(get_all_messages, _From, StateName, State) ->
  {reply, State#state.messages, StateName, State};

handle_sync_event(terminate, _From, _StateName, State) -> {stop, normal, ok, State};

handle_sync_event(_Event, _From, StateName, State) -> {reply, unknown, StateName, State}.

handle_info(_Message, StateName, State) -> {next_state, StateName, State}.

terminate(_Reason, _StateName, State) -> io:format("shutting down. state: ~p~n", [State]).

code_change(_OldVersion, StateName, State, _Extra) -> {ok, StateName, State}.

send(Recipients, Str) ->
  send(Recipients, Str, []).

send([], _Str, SendMessages) -> lists:reverse(SendMessages);

send([{_Name, Pid}|Rest], Message, SendMessages) ->
  gen_fsm:send_all_state_event(Pid, {message, Message}),
  send(Rest, Message, [Message|SendMessages]).
