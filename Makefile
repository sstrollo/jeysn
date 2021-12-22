
ERL_PREFIX = /opt/local

ERL  = $(ERL_PREFIX)/bin/erl
ERLC = $(ERL_PREFIX)/bin/erlc
ERTS_INCLUDE = $(wildcard $(ERL_PREFIX)/lib/erlang/erts-*/include)

PROPER = $(shell pwd)/../../proper


all: priv/ejson_nif.so ebin/ejson.beam test/jtest

.PHONY: all

ebin/%.beam: src/%.erl
	$(ERLC) -o $(dir $@) $<

test/%.beam: test/%.erl
	$(ERLC) -o $(dir $@) $<

c_src/%.o: c_src/%.c
	$(CC) -g -Wall -fPIC -fno-common -I$(ERTS_INCLUDE) -c $< -o $@

c_src/json.o: c_src/json.h
c_src/jtest.o: c_src/json.h

test/jtest.o: test/jtest.c
	$(CC) -Wall -g -Ic_src -c $< -o $@

test/jtest: test/jtest.o c_src/json.o
	$(CC) -o $@ $^

priv/ejson_nif.so: c_src/ejson.o c_src/json.o
	@mkdir -p $(dir $@)
	$(CC) -bundle -undefined dynamic_lookup -o $@ $^

clean:
	rm -f priv/*.so ebin/*.beam c_src/*.o
	rm -f test/*.beam test/*.o test/jtest test/jtest2


test: all test/ejson_test.beam
	cd test && $(ERL) -pa ../ebin -noshell -s ejson_test test


proper: all test/ejson_prop_test.beam
	cd test && \
	  $(ERL) -noshell -pa ../ebin -pz $(PROPER)/ebin -s ejson_prop_test test

proper_profile: all test/ejson_prop_test.beam
	cd test && \
	  $(ERL) -pa ../ebin -pz $(PROPER)/ebin -s ejson_prop_test profile

test/ejson_prop_test.beam: test/ejson_prop_test.erl
	$(ERLC) -I$(PROPER)/include -pz $(PROPER)/ebin -o $(dir $@) $<

erl: all
	env ERL_LIBS=`pwd`:$(PROPER) $(ERL) -pa `pwd`/test
