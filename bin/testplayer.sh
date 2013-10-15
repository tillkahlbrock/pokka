sleep 2
erl -pa /home/till/src/pokka/ebin/ -eval 'pokka_test_player:start_link("Timmey").' -noshell -detached
erl -pa /home/till/src/pokka/ebin/ -eval 'pokka_test_player:start_link("Jimmey").' -noshell -detached