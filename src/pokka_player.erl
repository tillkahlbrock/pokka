-module(pokka_player).
-behaviour(gen_server).
-include("pokka.hrl").

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

%%%===================================================================
%%% API
%%%===================================================================
start_link(Socket, Table) ->
  gen_server:start_link(?MODULE, [Socket, Table], []).

%%%===================================================================
%%% OTP callbacks
%%%===================================================================
init([Socket, Table]) ->
  gen_server:cast(self(), accept),
  {ok, #player_state{socket=Socket, table=Table}}.

handle_call(_Request, _From, State) ->
  {noreply, State}.

handle_cast(accept, State = #player_state{socket=ListenSocket}) ->
  {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
  {noreply, State#player_state{socket=AcceptSocket}};

handle_cast({_Type, Message}, State = #player_state{socket=Socket}) ->
  send(Socket, Message),
  {noreply, State};

handle_cast(_Msg, State) ->
  erlang:display("i dont know?!?"),
  {noreply, State}.

handle_info({tcp, _Port, Message = "JOIN "++_}, StateData) ->
  Name = extract_name_from_message(Message),
  log(Name ++ ": " ++ Message),
  gen_fsm:send_event(StateData#player_state.table, {join, #player{name=Name, pid=self()}}),
  {noreply, StateData#player_state{name=Name}};

handle_info({tcp, _Port, Message = "BLIND "++Amount}, StateData = #player_state{name=Name, table=Table}) ->
  log(Name ++ ": " ++ Message),
  gen_fsm:send_event(Table, {received_blind, Name, string:strip(string:strip(Amount, right, $\n), right, $\r)}),
  {noreply, StateData};

handle_info(Info, State) ->
  log(io_lib:format("unexpected: ~p~n", [Info])),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
send(Socket, Message) ->
  gen_tcp:send(Socket, Message ++ "\r\n"),
  inet:setopts(Socket, [{active, once}]).

tokens(String) -> string:tokens(String, "\r\n ").

extract_name_from_message(Msg) ->
  ["JOIN" | NameString] = tokens(Msg),
  Name = string:join(NameString, "_"),
  Name.

log(Message) ->
  file:write_file(?LOG_FILE, io_lib:fwrite("~p.\n", [Message]), [append]).