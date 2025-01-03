## Set some macro but not override environment ones
CFLAGS ?= -O2 -g -Wall -Werror
LDFLAGS ?=

all:: vfork_helper

clean::
	rm -f vfork_helper *.o

%.o: %.c
	gcc $(CFLAGS) -MMD -MP -MF $@.d -c -o $@ $<

vfork_helper: vfork_helper.o close_from.o syslog.o
	gcc $(CFLAGS) $(LDFLAGS) -o $@ $^ -pthread

-include $(wildcard *.o.d)

## Fuzzer uses AFL (American Fuzzy Lop).
##
## Use "make fuzz" to build and launch the fuzzer
##
## Use "make show" to look at the first failures (if found).

fuzz::
	afl-gcc $(CFLAGS) -Wall -Werror -o algo_fuzzer algo_fuzzer.c
	rm -rf testcase_dir
	mkdir testcase_dir
	echo maomaoamaoaoao > testcase_dir/test1
	rm -rf findings_dir/
	afl-fuzz -i testcase_dir -o findings_dir -D -- ./algo_fuzzer

show::
	cat "$$(ls -1 findings_dir/default/crashes/id* | head -1)" | ./algo_fuzzer
