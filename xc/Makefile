LDFLAGS = -cclib -L./

VERSION := $(shell git show-ref --head | grep -E " HEAD$$" | cut -f 1 -d " " || echo 0)

OCAMLABI := $(shell ocamlc -version)
OCAMLLIBDIR := $(shell ocamlc -where)
OCAMLDESTDIR ?= $(OCAMLLIBDIR)

CC = gcc
CFLAGS = -Wall -fPIC -O2 -I$(OCAMLLIBDIR) -I$(XEN_ROOT)/usr/include -I../mmap -I./
OCAMLC = ocamlc -g -I ../mmap -I ../uuid
OCAMLOPT = ocamlopt
OCAMLOPTFLAGS = -g -dtypes -I ../mmap -I ../uuid

OBJS = xc
INTF = xc.cmi
LIBS = xc.cma xc.cmxa

DOCDIR = ../doc

all: $(INTF) $(LIBS) $(PROGRAMS)

bins: $(PROGRAMS)

libs: $(LIBS)

xc.cmxa: libxc_stubs.a $(foreach obj,$(OBJS),$(obj).cmx)
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -a -o $@ -cclib -lxc_stubs $(foreach obj,$(OBJS),$(obj).cmx)

xc.cma: $(foreach obj,$(OBJS),$(obj).cmo)
	$(OCAMLC) -a -dllib dllxc_stubs.so -cclib -lxc_stubs -o $@ $(foreach obj,$(OBJS),$(obj).cmo)

xc_stubs.a: xc_lib.o xc_stubs.o
	ocamlmklib -o xc_stubs $+

libxc_stubs.a: xc_lib.o xc_stubs.o
	ar rcs $@ $+
	ocamlmklib -o xc_stubs $+

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
	ocamlfind install -destdir $(path) -ldconf ignore xc META $(INTF) $(LIBS) *.a *.so *.cmx

.PHONY: uninstall
uninstall:
	ocamlfind remove xc

.PHONY: doc
doc: $(INTF)
	python ../doc/doc.py $(DOCDIR) "xc" "package" "$(OBJS)" "." "mmap,uuid" ""

clean:
	rm -f *.o *.so *.a *.cmo *.cmi *.cma *.cmx *.cmxa *.annot $(LIBS) $(PROGRAMS) $(INTF) *~ *.rej *.orig

