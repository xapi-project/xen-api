FEPP = camlp4o -I ../rpc-light -I $(shell ocamlfind query type-conv) pa_type_conv.cmo pa_rpc.cma

LDFLAGS = -cclib -L./

VERSION := $(shell git show-ref --head | grep -E " HEAD$$" | cut -f 1 -d " " || echo 0)
OCAMLOPTFLAGS = -g -dtypes

OCAMLABI := $(shell ocamlc -version)
OCAMLLIBDIR := $(shell ocamlc -where)
OCAMLDESTDIR ?= $(OCAMLLIBDIR)

CC = gcc
CFLAGS = -Wall -fPIC -O2 -I$(OCAMLLIBDIR)
OCAMLC = ocamlc -g
OCAMLOPT = ocamlopt

OBJS = camldm
INTF = $(foreach obj, $(OBJS),$(obj).cmi)
LIBS = camldm.cma camldm.cmxa

DOCDIR = /myrepos/xen-api-libs/doc

OCAMLFLAGS = -pp '${FEPP}' -I ../rpc-light


all: $(INTF) $(LIBS) $(PROGRAMS)

bins: $(PROGRAMS)

libs: $(LIBS)

camldm.cmxa: libcamldm_stubs.a $(foreach obj,$(OBJS),$(obj).cmx)
	$(OCAMLOPT) $(OCAMLOPTFLAGS) $(OCAMLFLAGS) -a -o $@ -cclib -lcamldm_stubs -cclib -ldevmapper $(foreach obj,$(OBJS),$(obj).cmx)

camldm.cma: $(foreach obj,$(OBJS),$(obj).cmo)
	$(OCAMLC) $(OCAMLOPTFLAGS) $(OCAMLFLAGS) -a -dllib dllcamldm_stubs.so -cclib -lcamldm_stubs -cclib -ldevmapper -o $@ $(foreach obj,$(OBJS),$(obj).cmo)

camldm_stubs.a: camldm_stubs.o
	ocamlmklib -o camldm_stubs -ldevmapper $+

libcamldm_stubs.a: camldm_stubs.o
	ar rcs $@ $+
	ocamlmklib -o camldm_stubs -ldevmapper $+

%.cmo: %.ml
	$(OCAMLC) $(OCAMLFLAGS) -c -o $@ $<

%.cmi: %.mli
	$(OCAMLC) $(OCAMLFLAGS) -c -o $@ $<

%.cmx: %.ml
	$(OCAMLOPT) $(OCAMLOPTFLAGS) $(OCAMLFLAGS) -c -o $@ $<

%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $<

META: META.in
	sed 's/@VERSION@/$(VERSION)/g' < $< > $@

.PHONY: install
install: path = $(DESTDIR)$(shell ocamlfind printconf destdir)
install: $(LIBS) META
	mkdir -p $(path)
	ocamlfind install -destdir $(path) -ldconf ignore camldm META $(INTF) $(LIBS) *.a *.so *.cmx

.PHONY: uninstall
uninstall:
	ocamlfind remove camldm

.PHONY: doc
doc: $(INTF)
	python ../doc/doc.py $(DOCDIR) "camldm" "package" "$(OBJS)" "." "" ""

clean:
	rm -f *.o *.so *.a *.cmo *.cmi *.cma *.cmx *.cmxa *.annot $(LIBS) $(PROGRAMS)
