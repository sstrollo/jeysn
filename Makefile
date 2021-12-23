
ERL  = erl
ERLC = erlc

BEAMS = $(patsubst src/%.erl,ebin/%.beam,$(wildcard src/*.erl))

all: $(BEAMS)
	cd c_src && $(MAKE)

.PHONY: all

ebin/%.beam: src/%.erl
	@mkdir -p $(dir $@)
	$(ERLC) -o $(dir $@) $<

clean:
	cd c_src && $(MAKE) $@
	cd test  && $(MAKE) $@
	rm -rf ebin priv _build rebar.lock
