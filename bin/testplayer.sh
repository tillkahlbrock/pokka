sleep 2
erl -pa /home/till/src/pokka/ebin/ -eval 'pokka_app:start_player(timmey).' -noshell -detached
erl -pa /home/till/src/pokka/ebin/ -eval 'pokka_app:start_player(jimmey).' -noshell -detached