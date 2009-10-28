CC = gcc
CFLAGS = -Wall -fPIC -O2 -I/opt/xensource/lib/ocaml
OCAMLC = ocamlc -g -I ../xb/ -I ../xs/
OCAMLOPT = ocamlopt

LDFLAGS = -cclib -L./

DESTDIR ?= /
VERSION := $(shell hg parents --template "{rev}" 2>/dev/null || echo 0.0)
OCAMLOPTFLAGS = -g -dtypes -I ../xb/ -I ../xs/

OCAMLABI := $(shell ocamlc -version)
OCAMLLIBDIR := $(shell ocamlc -where)
OCAMLDESTDIR ?= $(OCAMLLIBDIR)

OBJS = xsrpc
INTF = xsrpc.cmi
LIBS = xsrpc.cma xsrpc.cmxa

DOCDIR = /myrepos/xen-api-libs.hg/doc

all: $(INTF) $(LIBS) $(PROGRAMS)

bins: $(PROGRAMS)

libs: $(LIBS)

xsrpc.cmxa: $(foreach obj,$(OBJS),$(obj).cmx)
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -a -o $@ $(foreach obj,$(OBJS),$(obj).cmx)

xsrpc.cma: $(foreach obj,$(OBJS),$(obj).cmo)
	$(OCAMLC) -a -o $@ $(foreach obj,$(OBJS),$(obj).cmo)

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
	ocamlfind install -destdir $(DESTDIR)$(shell ocamlfind printconf destdir) -ldconf ignore xsrpc META $(INTF) $(LIBS) *.a *.cmx

.PHONY: uninstall
uninstall:
	ocamlfind remove xsrpc
	
.PHONY: doc
doc: $(INTF)
	python ../doc/doc.py $(DOCDIR) "xsrpc" "library" "$(OBJS)" "." "xb,xs" ""
	
clean:
	rm -f *.o *.so *.a *.cmo *.cmi *.cma *.cmx *.cmxa *.annot META $(LIBS) $(PROGRAMS)

