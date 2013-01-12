.PHONY: all clean install build
all: build doc

NAME=forkexec
J=4

BINDIR ?= /usr/bin
SBINDIR ?= /usr/sbin
DESTDIR ?= /

export OCAMLRUNPARAM=b

setup.bin: setup.ml
	@ocamlopt.opt -o $@ $< || ocamlopt -o $@ $< || ocamlc -o $@ $<
	@rm -f setup.cmx setup.cmi setup.o setup.cmo

setup.data: setup.bin
	@./setup.bin -configure

build: setup.data setup.bin
	@./setup.bin -build -j $(J)

doc: setup.data setup.bin
	@./setup.bin -doc -j $(J)

install: setup.bin
	@./setup.bin -install
	install ./fe_main.native $(DESTDIR)/$(SBINDIR)/xcp-fe
	install ./fe_cli.native $(DESTDIR)/$(BINDIR)/xcp-fe-cli

test: setup.bin build
	@./setup.bin -test

reinstall: setup.bin
	@ocamlfind remove $(NAME) || true
	@./setup.bin -reinstall
	install ./fe_main.native $(DESTDIR)/$(SBINDIR)/xcp-fe
	install ./fe_cli.native $(DESTDIR)/$(BINDIR)/xcp-fe-cli

uninstall:
	@ocamlfind remove $(NAME) || true
	rm -f $(DESTDIR)/$(SBINDIR)/xcp-fe
	rm -f $(DESTDIR)/$(BINDIR)/xcp-fe-cli

clean:
	@ocamlbuild -clean
	@rm -f setup.data setup.log setup.bin
