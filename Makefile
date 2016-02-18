
PREFIX = /opt/local


ERL  = $(PREFIX)/bin/erl
ERLC = $(PREFIX)/bin/erlc
ERL_INCLUDE = $(wildcard $(PREFIX)/lib/erlang/erts-*/include)


all: eparse_nif.so eparse.beam jtest ejson_nif.so ejson.beam

%.beam: %.erl
	$(ERLC) $<

%.o: %.c
	$(CC) -g -Wall -fPIC -fno-common -I$(ERL_INCLUDE) -c $<

eparse_nif.so: eparse.o json.o
	$(CC) -bundle -undefined dynamic_lookup -o $@ $^

json.o: json.h
jtest.o: json.h

jtest.o: jtest.c
	$(CC) -Wall -g -c $<

jtest: jtest.o json.o
	$(CC) -o $@ $^

ejson_nif.so: ejson.o json.o
	$(CC) -bundle -undefined dynamic_lookup -o $@ $^

clean:
	rm -f jtest *.beam erl_crash.dump *.o *.so


test: all ejson_test.beam
	$(ERL) -noshell -s ejson_test test


PROPER = ../proper

proper: all ejson_xxx_test.beam
	$(ERL) -noshell -pz $(PROPER)/ebin -s ejson_xxx_test test

ejson_xxx_test.beam: ejson_xxx_test.erl
	$(ERLC) -I$(PROPER)/include -pz $(PROPER)/ebin $<

p: all ejson_xxx_test.beam
	$(ERL) -pz $(PROPER)/ebin
