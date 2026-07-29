# Standalone convenience Makefile for the ktls-helper.

CC      ?= gcc
CFLAGS  ?= -O2 -Wall -Wextra -Werror -std=gnu99
LDFLAGS ?=
LDLIBS  ?= -lssl -lcrypto

BIN     := ktls-helper

all: $(BIN)

$(BIN): ktls-helper.c
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $< $(LDLIBS)

install: $(BIN)
	install -D -m 0755 $(BIN) $(DESTDIR)/usr/libexec/xapi/$(BIN)

clean:
	rm -f $(BIN)

.PHONY: all install clean
