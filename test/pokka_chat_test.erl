%% Copyright
-module(pokka_chat_test).
-author("till").

-include_lib("eunit/include/eunit.hrl").

simple_test() ->
  %%TestPlayer = spawn(pokka_test_player, start, []),
  {ok, Table} = pokka_table:start_link(tablo, []),
  {ok, ListenSocket} = gen_tcp:listen(12345, [{active,once}, {packet,line}]),
  pokka_player:start_link(ListenSocket, Table),
  timer:sleep(2000),
  {_S, Helo} = connect(),
  ?_assertEqual(Helo, "SUCK ME BALLZ!").


connect() ->
  Socket = con(10),
  Helo = receive
           {tcp,_Socket,M1} -> M1
  after 2000 -> "received time while waiting for greeting."
         end,
  {Socket, Helo}.

con(0) -> exit('cant connect to server');

con(Tries) ->
  {Status, Socket} = gen_tcp:connect('localhost', 12345, [binary, {packet, 0}]),
  if
    Status == ok -> Socket;
    true -> timer:sleep(1000), con(Tries-1)
  end.