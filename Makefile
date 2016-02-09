
PREFIX = /opt/local


ERLC = $(PREFIX)/bin/erlc
ERL_INCLUDE = $(wildcard $(PREFIX)/lib/erlang/erts-*/include)


all: eparse_nif.so eparse.beam jtest

%.beam: %.erl
	$(ERLC) $<

%.o: %.c
	$(CC) -Wall -fPIC -fno-common -I$(ERL_INCLUDE) -c $<

eparse_nif.so: eparse.o json.o
	$(CC) -bundle -undefined dynamic_lookup -o $@ $^

json.o: json.h
jtest.o: json.h

jtest.o: jtest.c
	$(CC) -Wall -c $<

jtest: jtest.o json.o
	$(CC) -o $@ $^

clean:
	rm -f jtest *.beam *.o *.so
