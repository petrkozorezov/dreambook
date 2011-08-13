all: compile

compile:
	./rebar compile

clean:
	./rebar clean

release:
	./rebar generate

deps:
	./rebar get-deps

config:
	cp default.config.example default.config

