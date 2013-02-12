BINDIR?=/tmp/bin
ETCDIR?=/tmp/etc
INITDIR?=/tmp/etc/init.d

install:
	install -D -m 0755 dist/build/squeezed/squeezed ${BINDIR}/squeezed
	install -D -m 0644 scripts/squeezed.conf ${ETCDIR}/squeezed.conf
	install -D -m 0755 scripts/init.d-squeezed ${INITDIR}/squeezed
uninstall:
	rm -f ${BINDIR}/squeezed
	rm -f ${ETCDIR}/squeezed.conf
	rm -f ${INITDIR}/squeezed
