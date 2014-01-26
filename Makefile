
include config.mk

build: setup.data
	rm -f configure.cmo configure.cmi
	ocaml setup.ml -build

setup.data: setup.ml
	rm -f configure.cmo configure.cmi
	ocaml setup.ml -configure ${ENABLE_XENSERVER}

.PHONY: clean
clean: setup.data
	rm -f configure.cmo configure.cmi
	ocaml setup.ml -clean

install: build
	install -D -m 755 main.native ${BINDIR}/vhd-tool || echo "Failed to install vhd-tool"
	install -D -m 755 sparse_dd.native ${LIBEXECDIR}/sparse_dd || echo "Failed to install sparse_dd"
	install -D -m 644 src/sparse_dd.conf ${ETCDIR}/sparse_dd.conf || echo "Failed to install sparse_dd.conf"

config.mk:
	@echo Please run configure, try reading the help:
	./configure --help
	exit 1

.PHONY: distclean
distclean: clean
	rm -f config.mk
