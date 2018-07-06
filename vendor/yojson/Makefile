all:
	@jbuilder build @install @DEFAULT

test:
	@jbuilder runtest

check: test

install:
	@jbuilder install

uninstall:
	@jbuilder uninstall

bench:
	@jbuilder build bench/bench.exe

.PHONY: clean all bench test check install uninstall

clean:
	jbuilder clean
