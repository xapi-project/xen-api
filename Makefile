BINDIR ?= /usr/bin
SBINDIR ?= /usr/sbin
ETCDIR ?= /etc
<<<<<<< HEAD
=======
MANDIR ?= /usr/share/man/man1
>>>>>>> dea415eb2a75f8642c951151ae3f8ba6786f1cf9
all: build doc

.PHONY: test install uninstall clean

export OCAMLRUNPARAM=b
J=4

setup.bin: setup.ml
	@ocamlopt.opt -o $@ $< || ocamlopt -o $@ $< || ocamlc -o $@ $<
	@rm -f setup.cmx setup.cmi setup.o setup.cmo

setup.data: setup.bin
	@./setup.bin -configure --enable-tests

<<<<<<< HEAD
build: setup.data setup.bin
	@./setup.bin -build -j $(J)
=======
build: setup.data setup.bin networkd/version.ml
	@./setup.bin -build -j $(J)
	mv networkd.native xcp-networkd
	./xcp-networkd --help=groff > xcp-networkd.1

networkd/version.ml: VERSION
	echo "let version = \"$(shell cat VERSION)\"" > networkd/version.ml
>>>>>>> dea415eb2a75f8642c951151ae3f8ba6786f1cf9

doc: setup.data setup.bin
	@./setup.bin -doc -j $(J)

test: setup.bin build
	@./setup.bin -test

install:
<<<<<<< HEAD
	install -D networkd.native $(DESTDIR)$(SBINDIR)/xcp-networkd
	install -D networkd_db.native $(DESTDIR)$(BINDIR)/networkd_db
=======
	mkdir -p $(DESTDIR)$(SBINDIR)
	install xcp-networkd $(DESTDIR)$(SBINDIR)/xcp-networkd
	mkdir -p $(DESTDIR)$(MANDIR)
	install xcp-networkd.1 $(DESTDIR)$(MANDIR)/xcp-networkd.1
	mkdir -p $(DESTDIR)$(BINDIR)
	install networkd_db.native $(DESTDIR)$(BINDIR)/networkd_db
>>>>>>> dea415eb2a75f8642c951151ae3f8ba6786f1cf9

uninstall:
	rm -f $(DESTDIR)$(SBINDIR)/xcp-networkd
	rm -f $(DESTDIR)$(MANDIR)/xcp-networkd.1
	rm -f $(DESTDIR)$(SBINDIR)/networkd_db

clean:
	@ocamlbuild -clean
	@rm -f setup.data setup.log setup.bin
<<<<<<< HEAD
=======
	rm networkd/version.ml
	rm xcp-networkd xcp-networkd.1
>>>>>>> dea415eb2a75f8642c951151ae3f8ba6786f1cf9
