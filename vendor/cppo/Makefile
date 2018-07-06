all:
	@jbuilder build @install @DEFAULT

test:
	@jbuilder runtest

install:
	@jbuilder install

uninstall:
	@jbuilder uninstall

check: test

.PHONY: clean all check test install uninstall

clean:
	jbuilder clean
