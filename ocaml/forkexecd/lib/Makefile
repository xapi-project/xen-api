## Makefile for fe_stubs algorithm fuzzer.
## Fuzzer uses AFL (American Fuzzy Lop).
##
## Use "make" to build and launch the fuzzer
##
## Use "make show" to look at the first failures (if found).

fuzz::
	afl-gcc -O2 -Wall -Werror -g -o fe_stubs_algo_fuzzer fe_stubs_algo_fuzzer.c
	rm -rf testcase_dir
	mkdir testcase_dir
	echo maomaoamaoaoao > testcase_dir/test1
	rm -rf findings_dir/
	afl-fuzz -i testcase_dir -o findings_dir -- ./fe_stubs_algo_fuzzer

show::
	cat "$$(ls -1 findings_dir/default/crashes/id* | head -1)" | ./fe_stubs_algo_fuzzer

