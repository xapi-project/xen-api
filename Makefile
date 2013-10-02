
include config.mk

.PHONY: build
build: dist/setup
	obuild build

dist/setup: vhd-tool.obuild
	obuild configure

.PHONY: install
install: build
	install -D -m 755 dist/build/vhd-tool/vhd-tool ${BINDIR}/vhd-tool
	install -D -m 755 dist/build/sparse_dd/sparse_dd ${LIBEXECDIR}/sparse_dd
	install -D -m 644 src/sparse_dd.conf ${ETCDIR}/sparse_dd.conf

config.mk:
	@echo Please run configure, try reading the help:
	./configure --help
	exit 1

.PHONY: clean
clean:
	rm -rf dist
	rm -f configure.cmo configure.cmi

.PHONY: distclean
distclean: clean
	rm -f config.mk
