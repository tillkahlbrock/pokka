-module(pokka_player).
-behaviour(gen_fsm).
-export([start_link/2]).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
-export([startup/2, join/2, cards/2]).

start_link(Socket, Table) ->
  gen_fsm:start_link({local,Table}, ?MODULE, [Socket, Table], []).

init(StateData) ->
  gen_fsm:send_event(?MODULE, accept),
  {ok, startup, StateData}.

startup(accept, [ListenSocket, Table]) ->
  {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
  send(AcceptSocket, "Hello you! Wanna play some poker?", []),
  {next_state, join, [AcceptSocket, Table]}.

join(_Msg, StateData) ->
  {next_state, join, StateData}.

cards(_Msg, StateData) ->
  {next_state, cards, StateData}.

handle_event({status, Message}, StateName, StateData = [Socket | _]) ->
  send(Socket, Message, []),
  {next_state, StateName, StateData};

handle_event(_E, StateName, StateData) ->
  {next_state, StateName, StateData}.

handle_sync_event(_E, _From, StateName, StateData) ->
  {next_state, StateName, StateData}.

handle_info({tcp, _Port, Msg = "join "++_}, join, [Socket, Table]) ->
  ["join" | NameString] = tokens(Msg),
  Name = list_to_atom(string:join(NameString, "_")),
  Player = {Name, self()},
  ok = pokka:join_table(Table, Player),
  send(Socket, "Ok ~p, lets gamble!", [Name]),
  {next_state, cards, [Socket, Table, Name]};

handle_info({tcp, _Socket, "quit"++_}, join, StateData = [Socket, _Table]) ->
  gen_tcp:close(Socket),
  {stop, normal, StateData};

handle_info({tcp, _Socket, "quit"++_}, _StateName, StateData = [Socket, Table, Name]) ->
  gen_tcp:close(Socket),
  Player = {Name, self()},
  pokka:leave_table(Table, Player),
  {stop, normal, StateData};

handle_info({tcp, _Port, _Msg}, StateName, StateData = [Socket | _Rest]) ->
  send(Socket, "You don't know jack!'", []),
  {next_state, StateName, StateData};

handle_info({tcp_closed, _Socket, _}, _StateName, StateData) ->
  {stop, tcp_closed, StateData};

handle_info({tcp_error, _Socket, _}, _StateName, StateData) ->
  {stop, tcp_error, StateData};

handle_info(E, StateName, StateData) ->
  io:format("unexpected: ~p~n", [E]),
  {next_state, StateName, StateData}.

code_change(_OldVsn, _StateName, StateData, _Extra) ->
  {ok, StateData}.

terminate(normal, _StateName, _StateData) ->
  ok;

terminate(_Reason, _StateName, _StateData) ->
  io:format("terminate reason: ~p~n", [_Reason]).

send(Socket, Str, Args) ->
  ok = gen_tcp:send(Socket, io_lib:format(Str++"~n", Args)),
  ok = inet:setopts(Socket, [{active, once}]),
  ok.

tokens(String) -> string:tokens(String, "\r\n ").
