#!/bin/sh
export ERL_LIBS="./deps/:../:${ERL_LIBS}"
exec erl -pa ebin -s reloader -eval "application:load(dreambook)" -sname emacs
