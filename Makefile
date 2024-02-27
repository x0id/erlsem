all: compile

compile clean:
	rebar3 $@

test eunit:
	rebar3 eunit

.PHONY: test
