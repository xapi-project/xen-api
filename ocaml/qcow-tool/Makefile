
.PHONY: build clean test

build:
	dune build @install

test:
	dune build lib_test/compact_random.exe lib_test/test.exe
	./_build/default/lib_test/compact_random.exe -compact-mid-write -stop-after 16
	./_build/default/lib_test/test.exe -runner sequential

install:
	dune install

uninstall:
	dune uninstall

clean:
	dune clean
