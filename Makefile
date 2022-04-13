
all: rebar3
	./rebar3 compile

docs: rebar3
	./rebar3 edoc

eunit: rebar3
	./rebar3 $@

dialyzer: rebar3
	./rebar3 $@

proper: rebar3
	./rebar3 proper -n 5000

tests: rebar3
	./rebar3 dialyzer
	./rebar3 eunit
	./rebar3 proper -n 500

erl: rebar3
	./rebar3 shell

rebar3:
	curl -Os "https://s3.amazonaws.com/$@/$@" && chmod +x $@

.PHONY: all docs eunit dialyzer proper tests erl


clean:
	cd c_src && $(MAKE) $@
	rm -rf ebin priv _build

.PHONY: clean
