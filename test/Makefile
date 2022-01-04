
build: jtest jtest2 $(patsubst %.erl,%.beam,$(wilcard *.erl))

# ------------------------------------------------------------------------

ERL    ?= erl
ERLC   ?= erlc
PROPER ?= ../../proper

%.beam: %.erl
	$(ERLC) $<

test: jeysn_test.beam
	$(ERL) -pa ../ebin -noshell -s jeysn_test test

proper: jeysn_prop_test.beam
	$(ERL) -noshell -pa ../ebin -pz $(PROPER)/ebin -s jeysn_prop_test test

proper_profile: jeysn_prop_test.beam
	$(ERL) -pa ../ebin -pz $(PROPER)/ebin -s jeysn_prop_test profile

jeysn_prop_test.beam: jeysn_prop_test.erl
	$(ERLC) -I$(PROPER)/include -pz $(PROPER)/ebin $<

erl:
	env ERL_LIBS=`cd .. && pwd`:$(PROPER):$(ERL_LIBS) $(ERL)


# ------------------------------------------------------------------------

JDIR   = ../c_src

CFLAGS ?= -std=c99 -Wall -g
CFLAGS += -I$(JDIR)

jtest: jtest.o json.o
	$(CC) $^ $(LDFLAGS) $(LDLIBS) -o $@
jtest2: jtest2.o json.o
	$(CC) $^ $(LDFLAGS) $(LDLIBS) -o $@

jtest.o: $(JDIR)/json.h
jtest2.o: $(JDIR)/json.h
json.o: $(JDIR)/json.h

%.o: %.c
	$(CC) $(CFLAGS) $(CPPFLAGS) -c $<

%.o: $(JDIR)/%.c
	$(CC) $(CFLAGS) $(CPPFLAGS) -c $<

# ------------------------------------------------------------------------

clean:
	rm -f *.beam *.o jtest jtest2
	rm -f erl_crash.dump
