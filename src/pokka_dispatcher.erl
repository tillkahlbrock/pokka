-module(pokka_dispatcher).
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

handle_info({tcp, _Port, Msg}, [Socket, join]) ->
  ["join", Name] = tokens(Msg),
  ok = pokka:add_player(Name),
  send(Socket, "Ok ~p, lets gamble!", [Name]),
  {noreply, [Socket, cards]}.

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
