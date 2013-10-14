#!/bin/sh

erl -pa /home/till/src/pokka/ebin -sname pokka_app -s pokka_app -noshell -detached
sleep 3
erl -pa /home/till/src/pokka/ebin -sname player1 -s pokka_test_player start_link Timmey -noshell -detached
#sleep 1
#erl -pa /home/till/src/pokka/ebin -sname player1 -s pokka_test_player start_link Jimmey -noshell -detached