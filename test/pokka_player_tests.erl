-module(pokka_player_tests).
-include_lib("eunit/include/eunit.hrl").
-define(setup(Test), {setup, fun() -> start([socket, table_name]) end, fun stop/1, fun Test/1}).
-define(setup(InitStateData, Test), {setup, fun() -> start(InitStateData) end, fun stop/1, fun Test/1}).

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
join_new_player_test_() ->
  [
    {"It should send an async message (accept) to itself in the init funtions, "
      "so that the first action is to open a accept socket and not to be stuck in init",
    ?setup(send_accept_in_init)}
  ].

%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%
start(InitStateData) ->
  code:unstick_mod(gen_fsm),
  meck:new(gen_fsm),
  InitStateData.

stop(_InitStateData) ->
  meck:unload(gen_fsm).

%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%

%% Player joins
send_accept_in_init(InitStateData) ->
  meck:expect(gen_fsm, send_event, fun(Pid, Message) -> {ok, Pid, Message} end),
  Result = pokka_player:init(InitStateData),
  [
    ?_assert(meck:validate(gen_fsm)),
    ?_assert(meck:called(gen_fsm, send_event, [pokka_player, accept])),
    ?_assertEqual({ok, startup, InitStateData}, Result)
  ].
