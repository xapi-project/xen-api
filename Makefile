dist/build/lib-xenops/xenops.cmxa dist/build/list_domains/list_domains:
	obuild configure
	obuild build

BINDIR?=/usr/sbin

install:
	ocamlfind install xenops src/META $(wildcard dist/build/lib-xenops/*)
	install -m 755 dist/build/list_domains/list_domains ${BINDIR}

uninstall:
	ocamlfind remove xenops
	rm -f ${BINDIR}/list_domains

.PHONY: clean
clean:
	rm -rf dist
