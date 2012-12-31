-module(pokka_player).
-behaviour(gen_server).
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

start_link(Socket) ->
  gen_server:start_link(?MODULE, Socket, []).

init(Socket) ->
  gen_server:cast(self(), accept),
  {ok, [Socket]}.

%% No sync calls needed
handle_call(_E, _From, State) ->
  {noreply, State}.

handle_cast(accept, [ListenSocket]) ->
  {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
  send(AcceptSocket, "Hello you! Wanna play some poker?", []),
  {noreply, [AcceptSocket, join]}.

handle_info({tcp, _Port, Msg = "join "++_}, [Socket, join]) ->
  ["join", Name] = tokens(Msg),
  AtomName = list_to_atom(Name),
  ok = pokka:add_player(AtomName),
  send(Socket, "Ok ~p, lets gamble!", [AtomName]),
  {noreply, [Socket, cards, AtomName]};

handle_info({tcp, _Socket, "quit"++_}, State = [Socket, join]) ->
  gen_tcp:close(Socket),
  {stop, normal, State};

handle_info({tcp, _Socket, "quit"++_}, State = [Socket, _NextStep, Name]) ->
  gen_tcp:close(Socket),
  pokka:kill_player(Name),
  {stop, normal, State};

handle_info({tcp, _Port, _Msg}, State = [Socket, _NextStep]) ->
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
