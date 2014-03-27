all: compile

dev: dev_compile
	erl -sname unicorn -cookie unicorn -pa ebin -pa deps/etoml/ebin -pa deps/jiffy_v/ebin -s unicorn dev_start

compile:
	rebar compile

dev_compile:
	rebar -Crebar_dev.config get-deps
	rebar -Crebar_dev.config -DUNICORN_DEVEL compile

clean:
	rebar clean