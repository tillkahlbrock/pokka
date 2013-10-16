#!/bin/sh

erl -pa /home/till/src/pokka/ebin -sname pokka -s pokka_app -detached

sleep 2

erl -pa /home/till/src/pokka/ebin/ -eval 'pokka_app:start_player("Timmey").' -noshell -detached &
erl -pa /home/till/src/pokka/ebin/ -eval 'pokka_app:start_player("Jimmey").' -noshell -detached &