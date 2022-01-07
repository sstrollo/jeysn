
ERL  = erl
ERLC = erlc

BEAMS = $(patsubst src/%.erl,ebin/%.beam,$(wildcard src/*.erl))

all: rebar3
	./rebar3 compile

eunit:
	./rebar3 eunit

proper: rebar3
	./rebar3 proper -n 5000

tests: rebar3
	./rebar3 dialyzer
	./rebar3 eunit
	./rebar3 proper -n 500

erl: rebar3
	./rebar3 shell

rebar3:
	curl -O "https://s3.amazonaws.com/$@/$@" && chmod +x $@

.PHONY: all eunit proper tests erl


make: $(BEAMS)
	cd c_src && $(MAKE)

.PHONY: make


ebin/%.beam: src/%.erl
	@mkdir -p $(dir $@)
	$(ERLC) -o $(dir $@) $<


clean:
	cd c_src && $(MAKE) $@
	rm -rf ebin priv _build rebar.lock

.PHONY: clean
