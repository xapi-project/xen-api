include ../config.mk

CC = gcc
CFLAGS = -Wall -fPIC -O2 -I$(shell ocamlc -where)
OCAMLC = ocamlc -g
OCAMLOPT = ocamlopt

LDFLAGS = -cclib -L./

VERSION := $(shell git show-ref --head | grep -E " HEAD$$" | cut -f 1 -d " " || echo 0)
OCAMLOPTFLAGS = -g -dtypes

OCAMLABI := $(shell ocamlc -version)
OCAMLLIBDIR := $(shell ocamlc -where)
OCAMLDESTDIR ?= $(OCAMLLIBDIR)

OBJS = udev
INTF = $(foreach obj, $(OBJS),$(obj).cmi)
LIBS = udev.cma udev.cmxa

DOCDIR = ../doc

all: $(INTF) $(LIBS) $(PROGRAMS)

bins: $(PROGRAMS)

libs: $(LIBS)

udev.cmxa: libudev_stubs.a $(foreach obj,$(OBJS),$(obj).cmx)
	$(OCAMLOPT) $(OCAMLOPTFLAGS) $(OCAMLFLAGS) -a -o $@ -cclib -ludev_stubs $(foreach obj,$(OBJS),$(obj).cmx)

udev.cma: libudev_stubs.a $(foreach obj,$(OBJS),$(obj).cmo)
	$(OCAMLC) $(OCAMLFLAGS) -a -dllib dlludev_stubs.so -cclib -ludev_stubs -o $@ $(foreach obj,$(OBJS),$(obj).cmo)

udev_stubs.a: udev_stubs.o
	ocamlmklib -o udev_stubs $+

libudev_stubs.a: udev_stubs.o
	ar rcs $@ $+
	ocamlmklib -o udev_stubs $+

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
	ocamlfind install -destdir $(path) -ldconf ignore udev META $(INTF) $(LIBS) *.a *.so *.cmx *.sp?t

.PHONY: uninstall
uninstall:
	ocamlfind remove udev 

.PHONY: doc
doc: $(INTF)
	python ../doc/doc.py $(DOCDIR) "udev" "package" "$(OBJS)" "." "" ""

clean:
	rm -f *.o *.so *.a *.cmo *.cmi *.cma *.cmx *.cmxa *.annot *.sp?t $(LIBS) $(PROGRAMS)

