BINDIR?=/tmp/
MANDIR?=/tmp/

.PHONY: install uninstall clean

all: main.native main.1

main.native: setup.data version.ml
	ocaml setup.ml -build

setup.data: _oasis
	ocaml setup.ml -configure

version.ml: VERSION
	echo "let version = \"$(shell cat VERSION)\"" > version.ml

main.1: main.native
	./main.native --help=groff > main.1

install: main.native main.1
	install -m 0755 main.native ${BINDIR}/xapi-script-storage
	mkdir -p ${MANDIR}/man1
	install -m 0644 main.1 ${MANDIR}/man1/xapi-script-storage.1

uninstall:
	rm -f ${BINDIR}/xapi-script-storage
	rm -f ${MANDIR}/man1/xapi-script-storage.1

clean:
	rm -rf _build setup.data main.1 version.ml
