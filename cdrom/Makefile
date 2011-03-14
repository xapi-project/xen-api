include ../config.mk

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

OBJS = cdrom
INTF = $(foreach obj, $(OBJS),$(obj).cmi)
LIBS = cdrom.cma cdrom.cmxa

DOCDIR = /myrepos/xen-api-libs/doc

all: $(INTF) $(LIBS) $(PROGRAMS)

bins: $(PROGRAMS)

libs: $(LIBS)

cdrom.cmxa: libcdrom_stubs.a $(foreach obj,$(OBJS),$(obj).cmx)
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -a -o $@ -cclib -lcdrom_stubs $(foreach obj,$(OBJS),$(obj).cmx)

cdrom.cma: $(foreach obj,$(OBJS),$(obj).cmo)
	$(OCAMLC) -a -dllib dllcdrom_stubs.so -cclib -lcdrom_stubs -o $@ $(foreach obj,$(OBJS),$(obj).cmo)

cdrom_stubs.a: cdrom_stubs.o
	ocamlmklib -o cdrom_stubs $+

libcdrom_stubs.a: cdrom_stubs.o
	ar rcs $@ $+
	ocamlmklib -o cdrom_stubs $+

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
	ocamlfind install -destdir $(path) -ldconf ignore cdrom META $(INTF) $(LIBS) *.a *.so *.cmx *.sp?t

.PHONY: uninstall
uninstall:
	ocamlfind remove cdrom

.PHONY: doc
doc: $(INTF)
	python ../doc/doc.py $(DOCDIR) "cdrom" "package" "$(OBJS)" "." "" ""

clean:
	rm -f *.o *.so *.a *.cmo *.cmi *.cma *.cmx *.cmxa *.annot *.sp?t $(LIBS) $(PROGRAMS)
