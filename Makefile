
include config.mk

build: setup.data
	rm -f configure.cmo configure.cmi
	ocaml setup.ml -build
	rm -f vhd-tool
	ln -s main.native vhd-tool
	./vhd-tool --help=groff > vhd-tool.1
ifeq ($(ENABLE_XENSERVER), "--enable-xenserver")
	rm -f sparse_dd
	ln -s sparse_dd.native sparse_dd
	./sparse_dd --help=groff > sparse_dd.1
endif

setup.data: setup.ml
	rm -f configure.cmo configure.cmi
	ocaml setup.ml -configure ${ENABLE_XENSERVER}

.PHONY: clean
clean: setup.data
	rm -f configure.cmo configure.cmi
	ocaml setup.ml -clean
	rm -f vhd-tool
	rm -f sparse_dd

install: build
	mkdir -p ${BINDIR}
	install -m 755 main.native ${BINDIR}/vhd-tool || echo "Failed to install vhd-tool"
ifeq ($(ENABLE_XENSERVER), "--enable-xenserver")
	mkdir -p ${LIBEXECDIR}
	install -m 755 sparse_dd.native ${LIBEXECDIR}/sparse_dd || echo "Failed to install sparse_dd"
	mkdir -p ${ETCDIR}
	install -m 644 src/sparse_dd.conf ${ETCDIR}/sparse_dd.conf || echo "Failed to install sparse_dd.conf"
endif

.PHONY: uninstall
uninstall:
	rm -f ${BINDIR}/vhd-tool
ifeq ($(ENABLE_XENSERVER), "--enable-xenserver")
	rm -f ${LIBEXECDIR}/sparse_dd
	rm -f ${ETCDIR}/sparse_dd.conf
endif

config.mk:
	@echo Running './configure' with the defaults
	./configure

.PHONY: distclean
distclean: clean
	rm -f config.mk
