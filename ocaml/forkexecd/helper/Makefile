## Set some macro but not override environment ones
CFLAGS ?= -O2 -g
LDFLAGS ?=

syslogger: syslogger.c ../lib/close_from.c ../lib/close_from.h
	gcc $(CFLAGS) $(LDFLAGS) -Wall -Werror -o $@ $< ../lib/close_from.c
