
PREFIX = /opt/local


ERL  = $(PREFIX)/bin/erl
ERLC = $(PREFIX)/bin/erlc
ERL_INCLUDE = $(wildcard $(PREFIX)/lib/erlang/erts-*/include)


all: priv/ejson_nif.so ebin/ejson.beam test/jtest

ebin/%.beam: src/%.erl
	$(ERLC) -o $(dir $@) $<

test/%.beam: test/%.erl
	$(ERLC) -o $(dir $@) $<

c_src/%.o: c_src/%.c
	$(CC) -g -Wall -fPIC -fno-common -I$(ERL_INCLUDE) -c $< -o $@

priv/eparse_nif.so: c_src/eparse.o c_src/json.o
	$(CC) -bundle -undefined dynamic_lookup -o $@ $^

c_src/json.o: c_src/json.h
c_src/jtest.o: c_src/json.h

test/jtest.o: test/jtest.c
	$(CC) -Wall -g -Ic_src -c $< -o $@

test/jtest: test/jtest.o c_src/json.o
	$(CC) -o $@ $^

priv/ejson_nif.so: c_src/ejson.o c_src/json.o
	$(CC) -bundle -undefined dynamic_lookup -o $@ $^

clean:
	rm -f priv/*.so ebin/*.beam c_src/*.o
	rm -f test/*.beam test/*.o test/jtest


test: all test/ejson_test.beam
	cd test && $(ERL) -pa ../ebin -noshell -s ejson_test test


PROPER = $(shell pwd)/../proper

proper: all test/ejson_xxx_test.beam
	cd test && \
	  $(ERL) -noshell -pa ../ebin -pz $(PROPER)/ebin -s ejson_xxx_test test

test/ejson_xxx_test.beam: test/ejson_xxx_test.erl
	$(ERLC) -I$(PROPER)/include -pz $(PROPER)/ebin -o $(dir $@) $<

p: all ejson_xxx_test.beam
	$(ERL) -pz $(PROPER)/ebin
