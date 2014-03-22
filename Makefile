all: compile

dev: dev_compile
	erl -sname unicorn -cookie unicorn -pa ebin -pa deps/etoml/ebin -s unicorn dev_start

compile:
	rebar compile

dev_compile:
	rebar -DDEBUG compile

clean:
	rebar clean