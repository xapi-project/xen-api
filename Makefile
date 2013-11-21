
include config.mk

.PHONY: build_vhd_tool
build_vhd_tool:
	(cd vhd-tool && make)

.PHONY: build_sparse_dd
build_sparse_dd:
	(cd sparse_dd && make)

.PHONY: install_vhd_tool
install_vhd_tool:
	install -D -m 755 vhd-tool/dist/build/vhd-tool/vhd-tool ${BINDIR}/vhd-tool

.PHONY: install_sparse_dd
install_sparse_dd:
	install -D -m 755 sparse_dd/dist/build/sparse_dd/sparse_dd ${LIBEXECDIR}/sparse_dd
	install -D -m 644 src/sparse_dd.conf ${ETCDIR}/sparse_dd.conf

config.mk:
	@echo Please run configure, try reading the help:
	./configure --help
	exit 1

.PHONY: clean
clean:
	rm -rf vhd-tool/dist sparse_dd/dist
	rm -f configure.cmo configure.cmi

.PHONY: distclean
distclean: clean
	rm -f config.mk
