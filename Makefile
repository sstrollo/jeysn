
ERL  = erl
ERLC = erlc

BEAMS = $(patsubst src/%.erl,ebin/%.beam,$(wildcard src/*.erl))

all: rebar3
	./rebar3 compile

proper: rebar3
	./rebar3 proper -n 5000

rebar3:
	curl -O "https://s3.amazonaws.com/$@/$@" && chmod +x $@

.PHONY: all proper make


make: $(BEAMS)
	cd c_src && $(MAKE)

.PHONY: make


ebin/%.beam: src/%.erl
	@mkdir -p $(dir $@)
	$(ERLC) -o $(dir $@) $<


clean:
	cd c_src && $(MAKE) $@
	cd test  && $(MAKE) $@
	rm -rf ebin priv _build rebar.lock

.PHONY: clean
