.PHONY: all clean install build
all: build doc

NAME=xenops
J=4

BINDIR ?= /usr/bin
SBINDIR ?= /usr/sbin
LIBEXECDIR ?= /usr/lib/xcp/lib
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

install: setup.bin
	@./setup.bin -install
	install -D ./xenops_xc_main.native $(DESTDIR)/$(SBINDIR)/xenopsd
	install -D ./xenops_simulator_main.native $(DESTDIR)/$(SBINDIR)/xenopsd-simulator
	install -D ./xenguest_main.native $(DESTDIR)/$(LIBEXECDIR)/xenguest
	install -D ./xenopsd.conf $(DESTDIR)/$(ETCDIR)/xenopsd.conf

test: setup.bin build
	@./setup.bin -test

reinstall: setup.bin
	@ocamlfind remove $(NAME) || true
	@./setup.bin -reinstall
	install -D ./xenops_xc_main.native $(DESTDIR)/$(SBINDIR)/xenopsd
	install -D ./xenops_simulator_main.native $(DESTDIR)/$(SBINDIR)/xenopsd-simulator
	install -D ./xenguest_main.native $(DESTDIR)/$(LIBEXECDIR)/xenguest
	install -D ./xenopsd.conf $(DESTDIR)/$(ETCDIR)/xenopsd.conf

uninstall:
	@ocamlfind remove $(NAME) || true
	rm -f $(DESTDIR)/$(SBINDIR)/xenopsd
	rm -f $(DESTDIR)/$(SBINDIR)/xenopsd-simulator
	rm -f $(DESTDIR)/$(LIBEXECDIR)/xenguest
	rm -f $(DESTDIR)/$(ETCDIR)/xenopsd.conf

clean:
	@ocamlbuild -clean
	@rm -f setup.data setup.log setup.bin
