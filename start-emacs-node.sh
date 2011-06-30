#!/bin/sh
exec erl -pa ebin -s reloader -eval "application:load(dreambook)" -sname emacs
