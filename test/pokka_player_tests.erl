-module(pokka_player_tests).
-include_lib("eunit/include/eunit.hrl").
-define(setup(Test), {setup, fun() -> start([socket, table_name]) end, fun stop/1, fun Test/1}).
-define(setup(InitStateData, Test), {setup, fun() -> start(InitStateData) end, fun stop/1, fun Test/1}).

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
join_new_player_test_() ->
  [
    {"It should send an async message (accept) to itself in the init funtion, "
      "so that the first action is to open an accept socket and not to be stuck in init",
    ?setup(send_accept_in_init)},
    {"It should open an accept socket and return it",
    ?setup(open_accept_socket_and_return_it_in_startup)},
    {"It should send the status to the client",
    ?setup(send_status_to_the_client)},
    {"It should call pokka:join_table with the players name",
    ?setup(call_join_with_playername)},
    {"It should send an ack to the client",
    ?setup(send_ack_to_the_client)}
  ].

%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%
start(InitStateData) ->
  code:unstick_mod(gen_fsm),
  code:unstick_mod(gen_tcp),
  code:unstick_mod(inet),
  meck:new(gen_fsm),
  meck:new(gen_tcp),
  meck:new(inet),
  meck:new(pokka),
  meck:expect(gen_tcp, send, fun(_Socket, _Msg) -> ok end),
  meck:expect(gen_tcp, send, fun(_Socket, _Msg) -> ok end),
  meck:expect(inet, setopts, fun(_Socket, _Msg) -> ok end),
  InitStateData.

stop(_InitStateData) ->
  meck:unload(gen_tcp),
  meck:unload(gen_fsm),
  meck:unload(inet),
  meck:unload(pokka).

%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%

%% Startup phase
send_accept_in_init(InitStateData) ->
  meck:expect(gen_fsm, send_event, fun(Pid, Message) -> {ok, Pid, Message} end),
  Result = pokka_player:init(InitStateData),
  [
    ?_assert(meck:validate(gen_fsm)),
    ?_assert(meck:called(gen_fsm, send_event, ['_', accept])),
    ?_assertEqual({ok, startup, InitStateData}, Result)
  ].

open_accept_socket_and_return_it_in_startup(InitStateData = [Socket, TableName]) ->
  meck:expect(gen_tcp, accept, fun(_ListenSocket) -> {ok, accept_socket} end),
  NewStateData = [accept_socket, TableName],
  Result = pokka_player:startup(accept, InitStateData),
  [
    ?_assert(meck:validate(gen_tcp)),
    ?_assert(meck:called(gen_tcp, accept, [Socket])),
    ?_assertEqual({next_state, join, NewStateData}, Result)
  ].

send_status_to_the_client(InitStateData = [Socket|_]) ->
  Message = "some status message for the client",
  Result = pokka_player:handle_event({status, Message}, some_state, InitStateData),
  [
    ?_assert(meck:validate(gen_tcp)),
    ?_assert(meck:called(gen_tcp, send, [Socket, io_lib:format(Message++"~n", [])])),
    ?_assertEqual({next_state, some_state, InitStateData}, Result)
  ].

%% Player joins
call_join_with_playername(InitStateData = [_Socket, Tablename]) ->
  Playername = "Klaus",
  PlayernameAtom = list_to_atom(Playername),
  Message = "join "++Playername,
  meck:expect(pokka, join_table, fun(_TN,_PN) -> ok end),
  Result = pokka_player:handle_info({tcp, some_port, Message}, join, InitStateData),
  [
    ?_assert(meck:validate(pokka)),
    ?_assert(meck:called(pokka, join_table, [Tablename, {PlayernameAtom, '_'}])),
    ?_assertEqual({next_state, cards, InitStateData++[PlayernameAtom]}, Result)
  ].

send_ack_to_the_client(InitStateData = [Socket|_]) ->
  pokka_player:handle_info({tcp, some_port, some_message}, join, InitStateData),
  [
    ?_assert(meck:validate(gen_tcp)),
    ?_assert(meck:called(gen_tcp, send, [Socket, '_']))
  ].
