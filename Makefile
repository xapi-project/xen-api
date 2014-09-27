BINDIR ?= /usr/bin
SBINDIR ?= /usr/sbin
ETCDIR ?= /etc
all: build doc

.PHONY: test install uninstall clean

export OCAMLRUNPARAM=b
J=4

setup.bin: setup.ml
	@ocamlopt.opt -o $@ $< || ocamlopt -o $@ $< || ocamlc -o $@ $<
	@rm -f setup.cmx setup.cmi setup.o setup.cmo

setup.data: setup.bin
	@./setup.bin -configure --enable-tests

build: setup.data setup.bin networkd/version.ml
	@./setup.bin -build -j $(J)

networkd/version.ml: VERSION
	echo "let version = \"$(shell cat VERSION)\"" > networkd/version.ml

doc: setup.data setup.bin
	@./setup.bin -doc -j $(J)

test: setup.bin build
	@./setup.bin -test

install:
	install -D networkd.native $(DESTDIR)$(SBINDIR)/xcp-networkd
	install -D networkd_db.native $(DESTDIR)$(BINDIR)/networkd_db

uninstall:
	rm -f $(DESTDIR)$(SBINDIR)/xcp-networkd
	rm -f $(DESTDIR)$(SBINDIR)/networkd_db

clean:
	@ocamlbuild -clean
	@rm -f setup.data setup.log setup.bin
	rm networkd/version.ml
