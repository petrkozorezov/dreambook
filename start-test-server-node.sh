#!/bin/sh
export ERL_LIBS="./deps/:${ERL_LIBS}"
exec erl -pa ebin -boot start_sasl -s reloader -s dreambook -sname dreambook@`hostname` -config default
