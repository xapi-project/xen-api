IPROG=install -m 755 -o root -g root
CC = gcc
CFLAGS = -Wall -fPIC -O2 -I/opt/xensource/lib/ocaml
OCAMLC = ocamlc -g
OCAMLOPT = ocamlopt

LDFLAGS = -cclib -L./

LIBEXEC  = "/opt/xensource/libexec"
DESTDIR ?= /
VERSION := $(shell hg parents --template "{rev}" 2>/dev/null || echo 0.0)
OCAMLOPTFLAGS = -g -dtypes

OCAMLABI := $(shell ocamlc -version)
OCAMLLIBDIR := $(shell ocamlc -where)
OCAMLDESTDIR ?= $(OCAMLLIBDIR)

OBJS = closeandexec
INTF = $(foreach obj, $(OBJS),$(obj).cmi)
LIBS = closeandexec.cmxa closeandexec.cma

PROGRAMS = closeandexec

all: $(INTF) $(LIBS)

bins: $(PROGRAMS)

libs: $(LIBS)

closeandexec: closeandexec.cmxa all
	$(OCAMLOPT) $(OCAMLOPTFLAGS) closeandexec.cmxa -o $@ $<

closeandexec.cmxa: $(foreach obj,$(OBJS),$(obj).cmx)
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -a -o $@ $(foreach obj,$(OBJS),$(obj).cmx)

closeandexec.cma: $(foreach obj,$(OBJS),$(obj).cmo)
	$(OCAMLC) -a -o $@ $(foreach obj,$(OBJS),$(obj).cmo)

%.cmo: %.ml
	$(OCAMLC) -c -I ../stdext -thread -o $@ $<

%.cmi: %.mli
	$(OCAMLC) -c -I ../stdext -o $@ $<

%.cmx: %.ml
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c -I ../stdext -thread -o $@ $<

%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $<

META: META.in
	sed 's/@VERSION@/$(VERSION)/g' < $< > $@

.PHONY: install
install: $(LIBS) META
	ocamlfind install -destdir $(DESTDIR)$(shell ocamlfind printconf destdir) -ldconf ignore close-and-exec META $(INTF) $(LIBS) *.a *.cmx

.PHONY: bininstall
bininstall:
	mkdir -p $(DESDIR)$(LIBEXEC)
	$(IPROG) $(PROGRAMS) $(DESDIR)$(LIBEXEC)

.PHONY: uninstall
uninstall:
	ocamlfind remove close-and-exec

.PHONY: binuninstall
binuninstall:
	rm -f $(DESTDIR)$(LIBEXEC)$(PROGRAMS)

clean:
	rm -f *.o *.so *.a *.cmo *.cmi *.cma *.cmx *.cmxa *.annot $(LIBS) $(PROGRAMS)