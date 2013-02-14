.PHONY: all clean install build
all: build doc

NAME=xenops
J=4

BINDIR ?= /usr/bin
SBINDIR ?= /usr/sbin
LIBEXECDIR ?= /usr/lib/xcp/lib
SCRIPTSDIR ?= /usr/lib/xcp/scripts
ETCDIR ?= /etc
DESTDIR ?= /

export OCAMLRUNPARAM=b

TESTS     := --enable-tests
TESTS     := --disable-tests
XEN       ?= $(shell if ocamlfind query xenctrl >/dev/null 2>&1; then echo --enable-xen; fi)
XEN := --enable-xen
SIMULATOR := --enable-simulator

setup.bin: setup.ml
	@ocamlopt.opt -o $@ $< || ocamlopt -o $@ $< || ocamlc -o $@ $<
	@rm -f setup.cmx setup.cmi setup.o setup.cmo

setup.data: setup.bin
	@./setup.bin -configure $(TESTS) $(XEN) $(SIMULATOR)

build: setup.data setup.bin
	@./setup.bin -build -j $(J)

doc: setup.data setup.bin
	@./setup.bin -doc -j $(J)

install: setup.bin install-scripts
	@./setup.bin -install

.PHONY: install-scripts
install-scripts:
	install -D ./xenops_xc_main.native $(DESTDIR)/$(SBINDIR)/xenopsd
	install -D ./xenops_simulator_main.native $(DESTDIR)/$(SBINDIR)/xenopsd-simulator
	install -D ./xenguest_main.native $(DESTDIR)/$(LIBEXECDIR)/xenguest
	install -D ./scripts/vif $(DESTDIR)/$(SCRIPTSDIR)/vif
	install -D ./scripts/vncterm-wrapper $(DESTDIR)/$(LIBEXECDIR)/vncterm-wrapper
	install -D ./scripts/qemu-dm-wrapper $(DESTDIR)/$(LIBEXECDIR)/qemu-dm-wrapper
	install -D ./scripts/qemu-vif-script $(DESTDIR)/$(LIBEXECDIR)/qemu-vif-script
	install -D ./scripts/setup-vif-rules $(DESTDIR)/$(LIBEXECDIR)/setup-vif-rules
	install -D ./scripts/network.conf $(DESTDIR)/$(ETCDIR)/xcp/network.conf
	DESTDIR=$(DESTDIR) SBINDIR=$(SBINDIR) LIBEXECDIR=$(LIBEXECDIR) SCRIPTSDIR=$(SCRIPTSDIR) ./scripts/make-custom-xenopsd.conf

test: setup.bin build
	@./setup.bin -test

reinstall: setup.bin install-scripts
	@ocamlfind remove $(NAME) || true
	@./setup.bin -reinstall

uninstall:
	@ocamlfind remove $(NAME) || true
	rm -f $(DESTDIR)/$(SBINDIR)/xenopsd
	rm -f $(DESTDIR)/$(SBINDIR)/xenopsd-simulator
	rm -f $(DESTDIR)/$(LIBEXECDIR)/xenguest
	rm -f $(DESTDIR)/$(ETCDIR)/xenopsd.conf
	rm -f $(DESTDIR)/$(SCRIPTSDIR)/vif
	rm -f $(DESTDIR)/$(LIBEXECDIR)/vncterm-wrapper
	rm -f $(DESTDIR)/$(LIBEXECDIR)/qemu-dm-wrapper
	rm -f $(DESTDIR)/$(LIBEXECDIR)/setup-vif-rules
	rm -f $(DESTDIR)/$(ETCDIR)/xcp/network.conf

clean:
	@ocamlbuild -clean
	@rm -f setup.data setup.log setup.bin
