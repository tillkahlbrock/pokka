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

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link(Socket, Table) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Socket, Table) ->
  gen_server:start_link(?MODULE, [Socket, Table], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Socket, Table]) ->
  gen_server:cast(self(), accept),
  {ok, #player_state{socket=Socket, table=Table}}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(accept, State = #player_state{socket=ListenSocket}) ->
  {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
  {noreply, State#player_state{socket=AcceptSocket}};

handle_cast({_Type, Message}, State = #player_state{socket=Socket}) ->
  send(Socket, Message),
  {noreply, State};

handle_cast(_Msg, State) ->
  erlang:display("i dont know?!?"),
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
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