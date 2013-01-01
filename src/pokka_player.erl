-module(pokka_player).
-behaviour(gen_server).
-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

start_link(Socket, Table) ->
  gen_server:start_link(?MODULE, [Socket, Table], []).

init(State) ->
  gen_server:cast(self(), accept),
  {ok, State}.

%% No sync calls needed
handle_call(_E, _From, State) ->
  {noreply, State}.

handle_cast(accept, [ListenSocket, Table]) ->
  {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
  send(AcceptSocket, "Hello you! Wanna play some poker?", []),
  {noreply, [AcceptSocket, join, Table]};

handle_cast({status, Message}, State = [Socket | _]) ->
  send(Socket, Message, []),
  {noreply, State}.

handle_info({tcp, _Port, Msg = "join "++_}, [Socket, join, Table]) ->
  ["join" | Name] = tokens(Msg),
  AtomName = list_to_atom(string:join(Name, "_")),
  ok = pokka:join_table(Table, AtomName, self()),
  send(Socket, "Ok ~p, lets gamble!", [AtomName]),
  {noreply, [Socket, cards, AtomName, Table]};

handle_info({tcp, _Socket, "quit"++_}, State = [Socket, join, _Table]) ->
  gen_tcp:close(Socket),
  {stop, normal, State};

handle_info({tcp, _Socket, "quit"++_}, State = [Socket, _NextStep, Name, Table]) ->
  gen_tcp:close(Socket),
  pokka:leave_table(Table, Name),
  {stop, normal, State};

handle_info({tcp, _Port, _Msg}, State = [Socket | _Rest]) ->
  send(Socket, "You don't know jack!'", []),
  {noreply, State};

handle_info({tcp_closed, _Socket, _}, State) ->
  {stop, normal, State};

handle_info({tcp_error, _Socket, _}, State) ->
  {stop, normal, State};

handle_info(E, State) ->
  io:format("unexpected: ~p~n", [E]),
  {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(normal, _State) ->
    ok;

terminate(_Reason, _State) ->
    io:format("terminate reason: ~p~n", [_Reason]).

send(Socket, Str, Args) ->
  ok = gen_tcp:send(Socket, io_lib:format(Str++"~n", Args)),
  ok = inet:setopts(Socket, [{active, once}]),
  ok.

tokens(String) -> string:tokens(String, "\r\n ").
