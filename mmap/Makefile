CC = gcc
CFLAGS = -Wall -fPIC -O2 -I/usr/lib/ocaml
OCAMLC = ocamlc -g
OCAMLOPT = ocamlopt

LDFLAGS = -cclib -L./

VERSION := $(shell git show-ref --head | grep -E " HEAD$$" | cut -f 1 -d " " || echo 0)
OCAMLOPTFLAGS = -g -dtypes

OCAMLABI := $(shell ocamlc -version)
OCAMLLIBDIR := $(shell ocamlc -where)
OCAMLDESTDIR ?= $(OCAMLLIBDIR)

OBJS = mmap
INTF = $(foreach obj, $(OBJS),$(obj).cmi)
LIBS = mmap.cma mmap.cmxa

DOCDIR = /myrepos/xen-api-libs/doc

all: $(INTF) $(LIBS) $(PROGRAMS)

bins: $(PROGRAMS)

libs: $(LIBS)

mmap.cmxa: libmmap_stubs.a $(foreach obj,$(OBJS),$(obj).cmx)
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -a -o $@ -cclib -lmmap_stubs $(foreach obj,$(OBJS),$(obj).cmx)

mmap.cma: $(foreach obj,$(OBJS),$(obj).cmo)
	$(OCAMLC) -a -dllib dllmmap_stubs.so -cclib -lmmap_stubs -o $@ $(foreach obj,$(OBJS),$(obj).cmo)

mmap_stubs.a: mmap_stubs.o
	ocamlmklib -o mmap_stubs $+

libmmap_stubs.a: mmap_stubs.o
	ar rcs $@ $+
	ocamlmklib -o mmap_stubs $+

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
install: path = $(DESTDIR)$(shell ocamlfind printconf destdir)
install: $(LIBS) META
	mkdir -p $(path)
	ocamlfind install -destdir $(path) -ldconf ignore mmap META $(INTF) $(LIBS) *.a *.so *.cmx

.PHONY: uninstall
uninstall:
	ocamlfind remove mmap

.PHONY: doc
doc: $(INTF)
	python ../doc/doc.py $(DOCDIR) "mmap" "package" "$(OBJS)" "." "" ""

clean:
	rm -f *.o *.so *.a *.cmo *.cmi *.cma *.cmx *.cmxa *.annot $(LIBS) $(PROGRAMS)

