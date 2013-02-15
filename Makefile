BINDIR?=/tmp/bin
ETCDIR?=/tmp/etc
INITDIR?=/tmp/etc/init.d

dist/build/squeezed/squeezed:
	obuild configure
	obuild build

dist/build/test/test:
	obuild configure
	obuild build

test: dist/build/test/test
	./dist/build/test/test

install: dist/build/squeezed/squeezed
	install -D -m 0755 dist/build/squeezed/squeezed ${BINDIR}/squeezed
	install -D -m 0644 scripts/squeezed.conf ${ETCDIR}/squeezed.conf
	install -D -m 0755 scripts/init.d-squeezed ${INITDIR}/squeezed
.PHONY: uninstall
uninstall:
	rm -f ${BINDIR}/squeezed
	rm -f ${ETCDIR}/squeezed.conf
	rm -f ${INITDIR}/squeezed

.PHONY: clean
clean:
	rm -rf dist
