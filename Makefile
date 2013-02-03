BINDIR?=/tmp/

install:
	install -m 0755 dist/build/sm-cli/sm-cli ${BINDIR}

uninstall:
	rm -f ${BINDIR}/sm-cli
