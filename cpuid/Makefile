include ../config.mk

CC = gcc
CFLAGS = -Wall -fPIC -O2 -I/usr/lib/ocaml -I$(XEN_ROOT)/usr/include
OCAMLC = ocamlc -g
OCAMLOPT = ocamlopt
INCLUDES = -I ../stdext $(OCAML_XC_INC) -I ../xen-utils

LDFLAGS = -cclib -L./

VERSION := $(shell git show-ref --head | grep -E " HEAD$$" | cut -f 1 -d " " || echo 0)
OCAMLOPTFLAGS = -g -dtypes

OCAMLABI := $(shell ocamlc -version)
OCAMLLIBDIR := $(shell ocamlc -where)
OCAMLDESTDIR ?= $(OCAMLLIBDIR)

OBJS = cpuid
INTF = $(foreach obj, $(OBJS),$(obj).cmi)
LIBS = cpuid.cma cpuid.cmxa

DOCDIR = ../doc

all: $(INTF) $(LIBS) $(PROGRAMS)

bins: $(PROGRAMS)

libs: $(LIBS)

cpuid.cmxa: cpuid_stubs.a $(foreach obj,$(OBJS),$(obj).cmx)
	$(OCAMLOPT) $(OCAMLOPTFLAGS) $(INCLUDES) -a -o $@ -cclib -lcpuid_stubs $(foreach obj,$(OBJS),$(obj).cmx)

cpuid.cma: $(foreach obj,$(OBJS),$(obj).cmo)
	$(OCAMLC) -a -dllib dllcpuid_stubs.so -cclib -lcpuid_stubs $(INCLUDES) -o $@ $(foreach obj,$(OBJS),$(obj).cmo)

cpuid_stubs.a: cpuid_stubs.o
	ocamlmklib -o cpuid_stubs $+

libcpuid_stubs.a: cpuid_stubs.o
	ar rcs $@ $+
	ocamlmklib -o cpuid_stubs $+

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
	ocamlfind install -destdir $(path) -ldconf ignore cpuid META $(INTF) $(LIBS) *.a *.so *.cmx *.sp?t

.PHONY: uninstall
uninstall:
	ocamlfind remove cpuid

.PHONY: doc
doc: $(INTF)
	python ../doc/doc.py $(DOCDIR) "cpuid" "package" "$(OBJS)" "." "stdext xc xen-utils" ""

clean:
	rm -f *.o *.so *.a *.cmo *.cmi *.cma *.cmx *.cmxa *.annot *.sp?t $(LIBS) $(PROGRAMS) *~ *.rej

