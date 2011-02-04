IPROG=install -m 755
OCAMLPACKS = xmlm

CC = gcc
CFLAGS = -Wall -fPIC -O2 -I/opt/xensource/lib/ocaml

OCAMLFIND = ocamlfind
OCAMLFINDFLAGS = -package $(OCAMLPACKS)

OCAMLC = $(OCAMLFIND) ocamlc -g $(OCAMLFINDFLAGS)
OCAMLOPT = $(OCAMLFIND) ocamlopt $(OCAMLFINDFLAGS)

LDFLAGS = -cclib -L./

LIBEXEC  = "/opt/xensource/libexec"
VERSION := $(shell git show-ref --head | grep -E " HEAD$$" | cut -f 1 -d " " || echo 0)
OCAMLOPTFLAGS = -g -dtypes

OCAMLABI := $(shell ocamlc -version)
OCAMLLIBDIR := $(shell ocamlc -where)
OCAMLDESTDIR ?= $(OCAMLLIBDIR)

OBJS = xml
INTF = xml.cmi
LIBS = xml-light2.cma xml-light2.cmxa
PROGRAMS = xmlpp

DOCDIR = ../doc

all: $(INTF) $(LIBS)

bins: $(PROGRAMS)

libs: $(LIBS)

xmlpp: xmlpp.ml all
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -I ../stdext stdext.cmxa xml-light2.cmxa -linkpkg -o $@ $<

xml-light2.cmxa: $(foreach obj,$(OBJS),$(obj).cmx)
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -a -o $@ $(foreach obj,$(OBJS),$(obj).cmx)

xml-light2.cma: $(foreach obj,$(OBJS),$(obj).cmo)
	$(OCAMLC) -a -o $@ $(foreach obj,$(OBJS),$(obj).cmo)

%.cmo: %.ml
	$(OCAMLC) -c -I ../stdext -o $@ $<

%.cmi: %.mli
	$(OCAMLC) -c -I ../stdext -o $@ $<

%.cmx: %.ml
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c -I ../stdext -o $@ $<

%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $<

META: META.in
	sed 's/@VERSION@/$(VERSION)/g' < $< > $@

.PHONY: install
install: path = $(DESTDIR)$(shell ocamlfind printconf destdir)
install: $(LIBS) META
	mkdir -p $(path)
	ocamlfind install -destdir $(path) -ldconf ignore xml-light2 META $(INTF) $(LIBS) *.a *.cmx

.PHONY: bininstall
bininstall: path = $(DESTDIR)$(LIBEXEC)
bininstall: all
	mkdir -p $(path)
	$(IPROG) $(PROGRAMS) $(path)

.PHONY: uninstall
uninstall:
	ocamlfind remove xml-light2

.PHONY: binuninstall
binuninstall:
	rm -f $(DESTDIR)$(LIBEXEC)$(PROGRAMS)

.PHONY: doc
doc: $(INTF)
	python ../doc/doc.py $(DOCDIR) "xml-light2" "package" "$(OBJS)" "." "stdext,xmlm" ""

clean:
	rm -f *.o *.so *.a *.cmo *.cmi *.cma *.cmx *.cmxa *.annot $(LIBS) $(PROGRAMS)

