BINDIR?=/tmp/

.PHONY: build install uninstall clean

build: configure.done
	obuild build

configure.done:
	obuild configure
	touch configure.done

install:
	install -m 0755 dist/build/sm-cli/sm-cli ${BINDIR}

uninstall:
	rm -f ${BINDIR}/sm-cli

clean:
	obuild clean
	rm -f configure.done
