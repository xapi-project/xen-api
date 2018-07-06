# To be used by system package managers to bootstrap opam. topkg
# cannot be used as it needs opam-installer which is provided by opam
# itself.

# Typical usage:
#
# make all
# make install PREFIX=/usr/local
# make install-doc PREFIX=/usr/local

# Adjust the following on the cli invocation for configuring

PREFIX=/usr
LIBDIR=$(DESTDIR)$(PREFIX)/lib/ocaml/cmdliner
DOCDIR=$(DESTDIR)$(PREFIX)/share/doc/cmdliner
NATIVE=$(shell ocamlopt -version > /dev/null 2>&1 && echo true)

INSTALL=install
OCAMLBUILD=ocamlbuild -use-ocamlfind
B=_build/src
BASE=$(B)/cmdliner

ifeq ($(NATIVE),true)
	BUILD-TARGETS=build-byte build-native build-native-dynlink
	INSTALL-TARGETS=install-common install-byte install-native \
                 install-native-dynlink
else
	BUILD-TARGETS=build-byte
	INSTALL-TARGETS=install-common install-byte
endif

all: $(BUILD-TARGETS)

install: $(INSTALL-TARGETS)

install-doc:
	$(INSTALL) -d $(DOCDIR)
	$(INSTALL) CHANGES.md LICENSE.md README.md $(DOCDIR)

clean:
	$(OCAMLBUILD) -clean

build-byte:
	$(OCAMLBUILD) src/cmdliner.cma

build-native:
	$(OCAMLBUILD) src/cmdliner.cmxa

build-native-dynlink:
	$(OCAMLBUILD) src/cmdliner.cmxs

create-libdir:
	$(INSTALL) -d $(LIBDIR)

install-common: create-libdir
	$(INSTALL) pkg/META opam $(BASE).mli $(BASE).cmi $(BASE).cmti $(LIBDIR)

install-byte: create-libdir
	$(INSTALL) $(BASE).cma $(LIBDIR)

install-native: create-libdir
	$(INSTALL) $(BASE).cmxa $(BASE).a $(wildcard $(B)/cmdliner*.cmx) $(LIBDIR)

install-native-dynlink: create-libdir
	$(INSTALL) $(BASE).cmxs $(LIBDIR)

.PHONY: all install install-doc clean build-byte build-native \
	build-native-dynlink create-libdir install-common install-byte \
  install-native install-dynlink
