CC = gcc
CFLAGS = -Wall -fPIC -O2 -I/usr/lib/ocaml -I$(XEN_ROOT)/usr/include
OCAMLC = ocamlc -g
OCAMLOPT = ocamlopt

LDFLAGS = -cclib -L./

VERSION := $(shell git show-ref --head | grep -E " HEAD$$" | cut -f 1 -d " " || echo 0)
OCAMLOPTFLAGS = -g -dtypes

OCAMLABI := $(shell ocamlc -version)
OCAMLLIBDIR := $(shell ocamlc -where)
OCAMLDESTDIR ?= $(OCAMLLIBDIR)

OBJS = eventchn
INTF = $(foreach obj, $(OBJS),$(obj).cmi)
LIBS = eventchn.cma eventchn.cmxa

DOCDIR = /myrepos/xen-api-libs/doc

all: $(INTF) $(LIBS) $(PROGRAMS)

bins: $(PROGRAMS)

libs: $(LIBS)

eventchn.cmxa: libeventchn_stubs.a $(foreach obj,$(OBJS),$(obj).cmx)
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -a -o $@ -cclib -leventchn_stubs $(foreach obj,$(OBJS),$(obj).cmx)

eventchn.cma: $(foreach obj,$(OBJS),$(obj).cmo)
	$(OCAMLC) -a -dllib dlleventchn_stubs.so -cclib -leventchn_stubs -o $@ $(foreach obj,$(OBJS),$(obj).cmo)

eventchn_stubs.a: eventchn_stubs.o
	ocamlmklib -o eventchn_stubs $+

libeventchn_stubs.a: eventchn_stubs.o
	ar rcs $@ $+
	ocamlmklib -o eventchn_stubs $+

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
	ocamlfind install -destdir $(path) -ldconf ignore eventchn META $(INTF) $(LIBS) *.a *.so *.cmx *.sp?t

.PHONY: uninstall
uninstall:
	ocamlfind remove eventchn

.PHONY: doc
doc: $(INTF)
	python ../doc/doc.py $(DOCDIR) "eventchn" "package" "$(OBJS)" "." "" ""

clean:
	rm -f *.o *.so *.a *.cmo *.cmi *.cma *.cmx *.cmxa *.annot *.sp?t $(LIBS) $(PROGRAMS)

