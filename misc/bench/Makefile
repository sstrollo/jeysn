
ERL    ?= erl
ERLC   ?= erlc

BEAMS = $(patsubst %.erl,%.beam,$(wildcard *.erl))

JSON  = canada.json citm_catalog.json \
	array100000.json  array1000000.json \
	object100000.json object1000000.json \
	deeparray1000.json deepobject1000.json

# ------------------------------------------------------------------------

ERL_LIBS := ../../_build/default/lib/jeysn:./deps/deps:$(ERL_LIBS)

all: $(BEAMS) $(JSON)

json: $(JSON)

erl: $(BEAMS) $(JSON)
	env ERL_LIBS=$(ERL_LIBS) JSON="$(JSON)" $(ERL)

bench:  $(BEAMS) $(JSON)
	@env ERL_LIBS=$(ERL_LIBS) JSON="$(JSON)" $(ERL) \
		-noshell -s bench o -s init stop
	@echo ""
	@env ERL_LIBS=$(ERL_LIBS) JSON="$(JSON)" $(ERL) \
		-noshell -s bench l -s init stop

.PHONY: all json erl

%.beam: %.erl
	$(ERLC) $<

dloop2.beam: _dloop.hrl
dloop3.beam: _dloop.hrl

# ------------------------------------------------------------------------

canada.json:
	curl -s -O "https://raw.githubusercontent.com/mloskot/json_benchmark/master/data/canada.json"

citm_catalog.json:
	curl -s -O "https://raw.githubusercontent.com/RichardHightower/json-parsers-benchmark/master/data/citm_catalog.json"

array%.json:
	@echo "creating $@"
	@perl -e \
	  'print("[");for($$i=0;$$i<$*;$$i++){print(",")if($$i);print("1");}print("]")' > $@

deeparray%.json:
	@echo "creating $@"
	@perl -e \
	  'print("[");for($$i=0;$$i<$*;$$i++){print(",")if($$i);print("[");for($$j=0;$$j<$*;$$j++){print(",")if($$j);print("1");}print("]")}print("]");' > $@

object%.json:
	@echo "creating $@"
	@perl -e 'print("{");for($$i=0;$$i<$*;$$i++){print(",")if($$i);printf("\"a%09d\":1",$$i);}print("}")' > $@

deepobject%.json:
	@echo "creating $@"
	@perl -e 'print("{");for($$i=0;$$i<$*;$$i++){print(",")if($$i);printf("\"a%09d\":{",$$i);for($$j=0;$$j<$*;$$j++){print(",")if($$j);printf("\"a%09d\":1",$$j);}print("}")}print("}");' > $@

# ------------------------------------------------------------------------

clean:
	rm -f  *.beam *.json

# ------------------------------------------------------------------------

deps: deps/erlang.mk
	cd deps && $(MAKE) deps

.PHONY: deps

deps/erlang.mk:
	cd $(dir $@) && curl -Os https://erlang.mk/erlang.mk
