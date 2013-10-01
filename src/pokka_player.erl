-module(pokka_player).
-behaviour(gen_fsm).
-export([start_link/2, startup/2]).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-record(state, {socket, name, table}).

start_link(Socket, Table) ->
  gen_fsm:start_link(?MODULE, [Socket, Table], []).

init(StateData) ->
  gen_fsm:send_event(self(), accept),
  {ok, startup, StateData}.

startup(accept, [ListenSocket, Table]) ->
  {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
  {next_state, join, #state{socket=AcceptSocket, table=Table}}.

handle_event({message, Message}, StateName, StateData) ->
  send(StateData#state.socket, Message),
  {next_state, StateName, StateData};

handle_event(_E, StateName, StateData) ->
  {next_state, StateName, StateData}.

handle_sync_event(_E, _From, StateName, StateData) ->
  {next_state, StateName, StateData}.

handle_info({tcp, _Port, Msg = "JOIN "++_}, _State, StateData) ->
  ["JOIN" | NameString] = tokens(Msg),
  Name = list_to_atom(string:join(NameString, "_")),
  Player = {Name, self()},
  ok = pokka:join_table(StateData#state.table, Player),
  {next_state, cards, #state{socket=StateData#state.socket, table=StateData#state.table, name=Name}};

handle_info({tcp_closed, _Socket}, _StateName, S) ->
  {stop, normal, S};

handle_info({tcp_error, _Socket, _}, _StateName, S) ->
  {stop, normal, S};

handle_info(E, StateName, StateData) ->
  io:format("unexpected: ~p~n", [E]),
  {next_state, StateName, StateData}.

code_change(_OldVsn, _StateName, StateData, _Extra) ->
  {ok, StateData}.

terminate(Reason, _StateName, _StateData) ->
  io:format("terminate reason: ~p~n", [Reason]).

send(Socket, Message) ->
  gen_tcp:send(Socket, Message).

tokens(String) -> string:tokens(String, "\r\n ").