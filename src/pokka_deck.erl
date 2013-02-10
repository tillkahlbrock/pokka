-module(pokka_deck).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, pocket_cards/0]).

start_link() -> gen_server:start_link({local,?MODULE}, ?MODULE, [], []).

pocket_cards() ->
  gen_server:call(?MODULE, pocket_cards).

init([]) ->
  Deck = shuffle(build_deck()),
  {ok, Deck}.

handle_call(pocket_cards, _From, Deck) ->
  [Card1, Card2|RestDeck] = Deck,
  {reply, [Card1,Card2], RestDeck}.

handle_cast(_Request, Deck) ->
  {noreply, Deck}.

handle_info(_Request, Deck) ->
  {noreply, Deck}.

terminate(_Reason, Deck) -> io:format("shutting down. deck: ~p~n", [Deck]).

code_change(_OldVersion, Deck, _Extra) -> {ok, Deck}.

build_deck() ->
  Faces = [ace,king,queen,jack,ten,nine,eight,seven,six,five,four,three,two],
  Colors = [club,spade,heart,diamon],
  lists:flatten(
    lists:foldl(
      fun(Face,Deck) -> Deck++[lists:foldl(
        fun(Color,Acc) -> Acc++[{atom_to_list(Face)++atom_to_list(Color)}] end,
        [],
        Colors
        )] end,
      [],
      Faces
    )
  ).

shuffle(Deck) ->
  [X||{_,X} <- lists:sort([ {random:uniform(), N} || N <- Deck])].
