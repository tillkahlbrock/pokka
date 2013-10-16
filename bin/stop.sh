#!/bin/sh

erl -pa /home/till/src/pokka/ebin -s pokka_app stop -noshell -detached
sleep 3
killall beam.smp