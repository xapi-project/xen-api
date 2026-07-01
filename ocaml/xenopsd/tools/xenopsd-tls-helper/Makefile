# Standalone convenience Makefile for the xenopsd-tls-helper.
#
# The build is driven by dune (see ./dune) and the RPM spec.

CC      ?= gcc
CFLAGS  ?= -O2 -Wall -Wextra -Werror -std=gnu99
LDLIBS  ?= -lssl -lcrypto

BIN     := xenopsd-tls-helper

all: $(BIN)

$(BIN): xenopsd-tls-helper.c
	$(CC) $(CFLAGS) -o $@ $< $(LDLIBS)

install: $(BIN)
	install -D -m 0755 $(BIN) $(DESTDIR)/usr/libexec/xenopsd/$(BIN)

clean:
	rm -f $(BIN)

.PHONY: all install clean
