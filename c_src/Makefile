
SRC    = jeysn_ll.c json.c
OBJS   = $(SRC:.c=.o)
TARGET = ../priv/$(patsubst %.c,%.so,$(firstword $(SRC)))

PRINT_ERTS_DIR  = io:format("~ts/erts-~ts/include",
PRINT_ERTS_DIR +=           [code:root_dir(), erlang:system_info(version)]),
PRINT_ERTS_DIR += init:stop(0).

ERTS_INCLUDE_DIR ?= $(shell erl -noinput -eval '$(PRINT_ERTS_DIR)')

UNAME_SYS := $(shell uname -s)
ifeq ($(UNAME_SYS), Darwin)
  CC ?= cc
  CFLAGS ?= -O3 -std=c99 -finline-functions -Wall -Wmissing-prototypes
  LDFLAGS ?= -flat_namespace -undefined suppress
else ifeq ($(UNAME_SYS), FreeBSD)
  CC ?= cc
  CFLAGS ?= -O3 -std=c99 -finline-functions -Wall -Wmissing-prototypes
else
  CC ?= gcc
  CFLAGS ?= -O3 -std=c99 -finline-functions -Wall -Wmissing-prototypes
endif

CFLAGS += -fPIC -I$(ERTS_INCLUDE_DIR)

LDLIBS ?=
LDFLAGS += -shared

$(TARGET): $(OBJS)
	@mkdir -p $(dir $@)
	$(CC) $^ $(LDFLAGS) $(LDLIBS) -o $@

json.o: json.h
jeysn_ll.o: json.h

%.o: %.c
	$(CC) $(CFLAGS) $(CPPFLAGS) -c $<

clean:
	rm -f $(TARGET) $(OBJS)
