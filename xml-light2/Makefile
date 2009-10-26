OCAMLPACKS = xmlm

CC = gcc
CFLAGS = -Wall -fPIC -O2 -I/opt/xensource/lib/ocaml

OCAMLFIND = ocamlfind
OCAMLFINDFLAGS = -package $(OCAMLPACKS)

OCAMLC = $(OCAMLFIND) ocamlc -g $(OCAMLFINDFLAGS)
OCAMLOPT = $(OCAMLFIND) ocamlopt $(OCAMLFINDFLAGS)

LDFLAGS = -cclib -L./

DESTDIR ?= /
VERSION := $(shell hg parents --template "{rev}" 2>/dev/null || echo 0.0)
OCAMLOPTFLAGS = -g -dtypes

OCAMLABI := $(shell ocamlc -version)
OCAMLLIBDIR := $(shell ocamlc -where)
OCAMLDESTDIR ?= $(OCAMLLIBDIR)

OBJS = xml
INTF = xml.cmi
LIBS = xml-light2.cma xml-light2.cmxa
PROGRAMS = xmlpp

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
install: $(LIBS) META
	ocamlfind install -destdir $(DESTDIR)$(shell ocamlfind printconf destdir) -ldconf ignore xml-light2 META $(INTF) $(LIBS) *.a *.cmx

.PHONY: uninstall
uninstall:
	ocamlfind remove xml-light2

clean:
	rm -f *.o *.so *.a *.cmo *.cmi *.cma *.cmx *.cmxa *.annot $(LIBS) $(PROGRAMS)

