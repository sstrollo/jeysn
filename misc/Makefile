
BEAMS = $(patsubst %.erl,%.beam,$(wildcard *.erl))

build: jtest jtest2 $(BEAMS)

# ------------------------------------------------------------------------

ERL    ?= erl
ERLC   ?= erlc

%.beam: %.erl
	$(ERLC) $<

# ------------------------------------------------------------------------

erl: $(BEAMS)
	env ERL_LIBS=`cd ../_build/default/lib && pwd`:$(ERL_LIBS) $(ERL)

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
