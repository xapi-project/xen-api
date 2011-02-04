CC = gcc
CFLAGS = -Wall -fPIC -O2 -I/usr/lib/ocaml
OCAMLC = ocamlc -g
OCAMLOPT = ocamlopt

LDFLAGS = -cclib -L./

VERSION := $(shell git show-ref --head | grep -E " HEAD$$" | cut -f 1 -d " " || echo 0)
OCAMLOPTFLAGS = -g -dtypes
OCAMLFLAGS = -I ../stdext

OCAMLABI := $(shell ocamlc -version)
OCAMLLIBDIR := $(shell ocamlc -where)
OCAMLDESTDIR ?= $(OCAMLLIBDIR)

OBJS = netdev
INTF = $(foreach obj, $(OBJS),$(obj).cmi)
LIBS = netdev.cma netdev.cmxa

DOCDIR = ../doc

all: $(INTF) $(LIBS) $(PROGRAMS)

bins: $(PROGRAMS)

libs: $(LIBS)

netdev.cmxa: libnetdev_stubs.a $(foreach obj,$(OBJS),$(obj).cmx)
	$(OCAMLOPT) $(OCAMLOPTFLAGS) $(OCAMLFLAGS) -a -o $@ -cclib -lnetdev_stubs $(foreach obj,$(OBJS),$(obj).cmx)

netdev.cma: libnetdev_stubs.a $(foreach obj,$(OBJS),$(obj).cmo)
	$(OCAMLC) $(OCAMLFLAGS) -a -dllib dllnetdev_stubs.so -cclib -lnetdev_stubs -o $@ $(foreach obj,$(OBJS),$(obj).cmo)

netdev_stubs.a: addr_stubs.o bridge_stubs.o link_stubs.o
	ocamlmklib -o netdev_stubs $+

libnetdev_stubs.a: addr_stubs.o bridge_stubs.o link_stubs.o
	ar rcs $@ $+
	ocamlmklib -o netdev_stubs $+

%.cmo: %.ml
	$(OCAMLC) $(OCAMLFLAGS) -c -o $@ $<

%.cmi: %.mli
	$(OCAMLC) $(OCAMLFLAGS) -c -o $@ $<

%.cmx: %.ml
	$(OCAMLOPT) $(OCAMLFLAGS) $(OCAMLOPTFLAGS) -c -o $@ $<

%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $<

META: META.in
	sed 's/@VERSION@/$(VERSION)/g' < $< > $@

.PHONY: install
install: path = $(DESTDIR)$(shell ocamlfind printconf destdir)
install: $(LIBS) META
	mkdir -p $(path)
	ocamlfind install -destdir $(path) -ldconf ignore netdev META $(INTF) $(LIBS) *.a *.so *.cmx

.PHONY: uninstall
uninstall:
	ocamlfind remove netdev 

.PHONY: doc
doc: $(INTF)
	python ../doc/doc.py $(DOCDIR) "netdev" "package" "$(OBJS)" "." "" ""

clean:
	rm -f *.o *.so *.a *.cmo *.cmi *.cma *.cmx *.cmxa *.annot $(LIBS) $(PROGRAMS)

