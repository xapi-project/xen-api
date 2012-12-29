.PHONY: all clean install build
all: build doc

NAME=xenops
J=4

BINDIR ?= /usr/bin
SBINDIR ?= /usr/sbin
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
	install ./xenops_main.native $(DESTDIR)/$(SBINDIR)/xenopsd
	install ./xenops_simulator_main.native $(DESTDIR)/$(SBINDIR)/xenopsd-simulator

test: setup.bin build
	@./setup.bin -test

reinstall: setup.bin
	@ocamlfind remove $(NAME) || true
	@./setup.bin -reinstall
	install ./xenops_main.native $(DESTDIR)/$(SBINDIR)/xenopsd
	install ./xenops_simulator_main.native $(DESTDIR)/$(SBINDIR)/xenopsd-simulator

uninstall:
	@ocamlfind remove $(NAME) || true
	rm -f $(DESTDIR)/$(SBINDIR)/xenopsd
	rm -f $(DESTDIR)/$(SBINDIR)/xenopsd-simulator

clean:
	@ocamlbuild -clean
	@rm -f setup.data setup.log setup.bin
