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

%% Accepting a connection
handle_cast(accept, [ListenSocket]) ->
  {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
  %% Reply to incomming connections here with gen_tcp:send(AcceptSocket, Msg)
  {noreply, [AcceptSocket]}.

handle_info({tcp, _Port, Msg}, S = [Socket]) ->
  Name = Msg, %% Extract name from message here
  pokka:kill_player(Name),
  gen_tcp:close(Socket),
  {stop, normal, S}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(normal, _State) ->
    ok;

terminate(_Reason, _State) ->
    io:format("terminate reason: ~p~n", [_Reason]).
