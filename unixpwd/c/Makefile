#
# Makefile to build a stand-alone binary for testing
#

CC 		= clang
CC 		= gcc
CFLAGS 	= -g -DDEVELOPMENT
CFLAGS 	= -g -Wall -fsanitize=address
CFLAGS 	= -g -Wall

VOPTS   += --track-fds=yes
VOPTS   += --tool=memcheck
VOPTS   += --leak-check=full
VOPTS   += --show-leak-kinds=all
VOPTS   += --track-origins=yes

.PHONY: all clean indent valgrind

all: 	unixpwd

unixpwd: main.o unixpwd.o
		$(CC) $(CFLAGS) -o unixpwd unixpwd.o main.o

clean:
		rm -f unixpwd
		rm -f *.o
		rm -f shadow.* passwd.*

indent:
		indent -orig -nut unixpwd.c unixpwd.h unixpwd_stubs.c main.c

valgrind: unixpwd
		valgrind $(VOPTS) ./unixpwd root || true
		valgrind $(VOPTS) ./unixpwd root foo || true
		valgrind $(VOPTS) ./unixpwd foo || true
		valgrind $(VOPTS) ./unixpwd > /dev/null || true
