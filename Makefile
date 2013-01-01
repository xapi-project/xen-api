.PHONY: all clean install build
all: build doc

J=4

BINDIR ?= /usr/bin
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
	install -D ./main.native $(DESTDIR)/$(BINDIR)/xenops-cli

test: setup.bin build
	@./setup.bin -test

reinstall: setup.bin
	@./setup.bin -reinstall
	install -D ./main.native $(DESTDIR)/$(BINDIR)/xenops-cli

uninstall:
	rm -f $(DESTDIR)/$(BINDIR)/xenops-cli

clean:
	@ocamlbuild -clean
	@rm -f setup.data setup.log setup.bin
