CC = gcc
CFLAGS = -Wall -fPIC -O2 -I/opt/xensource/lib/ocaml -I$(XEN_ROOT)/usr/include
OCAMLC = ocamlc -g
OCAMLOPT = ocamlopt
INCLUDES = -I ../stdext

LDFLAGS = -cclib -L./

VERSION := $(shell git show-ref --head | grep -E " HEAD$" | cut -f 1 -d " " || echo 0)
OCAMLOPTFLAGS = -g -dtypes

OCAMLABI := $(shell ocamlc -version)
OCAMLLIBDIR := $(shell ocamlc -where)
OCAMLDESTDIR ?= $(OCAMLLIBDIR)

OBJS = xen_cmdline
INTF = $(foreach obj, $(OBJS),$(obj).cmi)
LIBS = xen-utils.cma xen-utils.cmxa

DOCDIR = /myrepos/xen-api-libs/doc

all: $(INTF) $(LIBS) $(PROGRAMS)

bins: $(PROGRAMS)

libs: $(LIBS)

xen-utils.cmxa: $(foreach obj,$(OBJS),$(obj).cmx)
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -a -o $@ $(foreach obj,$(OBJS),$(obj).cmx)

xen-utils.cma: $(foreach obj,$(OBJS),$(obj).cmo)
	$(OCAMLC) -a -o $@ $(foreach obj,$(OBJS),$(obj).cmo)

%.cmo: %.ml
	$(OCAMLC) $(INCLUDES) -c -o $@ $<

%.cmi: %.mli
	$(OCAMLC) $(INCLUDES) -c -o $@ $<

%.cmx: %.ml
	$(OCAMLOPT) $(OCAMLOPTFLAGS) $(INCLUDES) -c -o $@ $<

%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $<

META: META.in
	sed 's/@VERSION@/$(VERSION)/g' < $< > $@

.PHONY: install
install: path = $(DESTDIR)$(shell ocamlfind printconf destdir)
install: $(LIBS) META
	mkdir -p $(path)
	ocamlfind install -destdir $(path) -ldconf ignore xen-utils META $(INTF) $(LIBS) *.a *.cmx

.PHONY: uninstall
uninstall:
	ocamlfind remove xen-utils

.PHONY: doc
doc: $(INTF)
	python ../doc/doc.py $(DOCDIR) "xen-utils" "package" "$(OBJS)" "." "stdext" ""

clean:
	rm -f *.o *.so *.a *.cmo *.cmi *.cma *.cmx *.cmxa *.annot $(LIBS) $(PROGRAMS)

