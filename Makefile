SETUP = ocaml setup.ml

build: setup.data
	$(SETUP) -build $(BUILDFLAGS)

doc: setup.data build
	$(SETUP) -doc $(DOCFLAGS)

test: setup.data build
	$(SETUP) -test $(TESTFLAGS)

all:
	$(SETUP) -all $(ALLFLAGS)

install: setup.data
	$(SETUP) -install $(INSTALLFLAGS)

uninstall:
	ocamlfind remove xml-light2
	ocamlfind remove xenstore-compat
	ocamlfind remove xenctrlext
	ocamlfind remove xen-utils
	ocamlfind remove uuid
	ocamlfind remove stunnel
	ocamlfind remove sha1
	ocamlfind remove sexpr
	ocamlfind remove pciutil
	ocamlfind remove http-svr
	ocamlfind remove gzip

reinstall: uninstall
	$(SETUP) -install $(REINSTALLFLAGS)

clean:
	$(SETUP) -clean $(CLEANFLAGS)

distclean:
	$(SETUP) -distclean $(DISTCLEANFLAGS)

setup.data: setup.ml
	$(SETUP) -configure $(CONFIGUREFLAGS)

configure:
	$(SETUP) -configure $(CONFIGUREFLAGS)

setup.ml: _oasis
	oasis setup

.PHONY: build doc test all install uninstall reinstall clean distclean configure
