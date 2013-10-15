#!/bin/sh

erl -pa /home/till/src/pokka/ebin -sname pokka -s pokka_app -detached

/bin/sh /home/till/src/pokka/bin/testplayer.sh &