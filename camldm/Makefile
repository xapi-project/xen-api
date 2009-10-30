
CC = gcc
CFLAGS = -Wall -fPIC -O2 -I/opt/xensource/lib/ocaml
OCAMLC = ocamlc -g
OCAMLOPT = ocamlopt

LDFLAGS = -cclib -L./

DESTDIR ?= /
VERSION := $(shell hg parents --template "{rev}" 2>/dev/null || echo 0.0)
OCAMLOPTFLAGS = -g -dtypes

OCAMLABI := $(shell ocamlc -version)
OCAMLLIBDIR := $(shell ocamlc -where)
OCAMLDESTDIR ?= $(OCAMLLIBDIR)

OBJS = camldm
INTF = $(foreach obj, $(OBJS),$(obj).cmi)
LIBS = camldm.cma camldm.cmxa

DOCDIR = /myrepos/xen-api-libs.hg/doc

all: $(INTF) $(LIBS) $(PROGRAMS)

bins: $(PROGRAMS)

libs: $(LIBS)

camldm.cmxa: libcamldm_stubs.a $(foreach obj,$(OBJS),$(obj).cmx)
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -a -o $@ -cclib -lcamldm_stubs -cclib -ldevmapper $(foreach obj,$(OBJS),$(obj).cmx)

camldm.cma: $(foreach obj,$(OBJS),$(obj).cmo)
	$(OCAMLC) -a -dllib dllcamldm_stubs.so -cclib -lcamldm_stubs -cclib -ldevmapper -o $@ $(foreach obj,$(OBJS),$(obj).cmo)

camldm_stubs.a: camldm_stubs.o
	ocamlmklib -o camldm_stubs -ldevmapper $+

libcamldm_stubs.a: camldm_stubs.o
	ar rcs $@ $+
	ocamlmklib -o camldm_stubs -ldevmapper $+

%.cmo: %.ml
	$(OCAMLC) -c -o $@ $<

%.cmi: %.mli
	$(OCAMLC) -c -o $@ $<

%.cmx: %.ml
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c -o $@ $<

%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $<

META: META.in
	sed 's/@VERSION@/$(VERSION)/g' < $< > $@

.PHONY: install
install: $(LIBS) META
	ocamlfind install -destdir $(DESTDIR)$(shell ocamlfind printconf destdir) -ldconf ignore camldm META $(INTF) $(LIBS) *.a *.so *.cmx

.PHONY: uninstall
uninstall:
	ocamlfind remove camldm

.PHONY: doc
doc: $(INTF)
	python ../doc/doc.py $(DOCDIR) "camldm" "package" "$(OBJS)" "." "" ""
	
clean:
	rm -f *.o *.so *.a *.cmo *.cmi *.cma *.cmx *.cmxa *.annot $(LIBS) $(PROGRAMS)
