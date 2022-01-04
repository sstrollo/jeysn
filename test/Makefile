
BEAMS = $(patsubst %.erl,%.beam,$(filter-out prop_%,$(wildcard *.erl)))

build: jtest jtest2 $(BEAMS)

# ------------------------------------------------------------------------

ERL    ?= erl
ERLC   ?= erlc

%.beam: %.erl
	$(ERLC) $<

test: jeysn_test.beam
	$(ERL) -pa ../ebin -noshell -s jeysn_test test

# ------------------------------------------------------------------------

PROPER ?= ../../proper

proper: prop_jeysn.beam
	$(ERL) -noshell -pa ../ebin -pz $(PROPER)/ebin -s prop_jeysn test

proper_profile: prop_jeysn.beam
	$(ERL) -pa ../ebin -pz $(PROPER)/ebin -s prop_jeysn profile

prop_jeysn.beam: prop_jeysn.erl
	$(ERLC) -I$(PROPER)/include -pz $(PROPER)/ebin $<

# ------------------------------------------------------------------------

erl: $(BEAMS)
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
