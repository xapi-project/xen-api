CC = gcc
CFLAGS = -Wall -fPIC -O2 -I/opt/xensource/lib/ocaml
OCAMLC = ocamlc -g -I ../xb/
OCAMLOPT = ocamlopt

LDFLAGS = -cclib -L./

DESTDIR ?= /
VERSION := $(shell hg parents --template "{rev}" 2>/dev/null || echo 0.0)
OCAMLOPTFLAGS = -g -dtypes -I ../xb/

OCAMLABI := $(shell ocamlc -version)
OCAMLLIBDIR := $(shell ocamlc -where)
OCAMLDESTDIR ?= $(OCAMLLIBDIR)

PREINTF = xsraw.cmi xst.cmi
PREOBJS = queueop xsraw xst
PRELIBS = $(foreach obj, $(PREOBJS),$(obj).cmo) $(foreach obj,$(PREOJBS),$(obj).cmx)
OBJS = queueop xsraw xst xs
INTF = xsraw.cmi xst.cmi xs.cmi
LIBS = xs.cma xs.cmxa

DOCDIR = /myrepos/xen-api-libs.hg/doc

all: $(PREINTF) $(PRELIBS) $(INTF) $(LIBS) $(PROGRAMS)

bins: $(PROGRAMS)

libs: $(LIBS)

xs.cmxa: $(foreach obj,$(OBJS),$(obj).cmx)
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -a -o $@ $(foreach obj,$(OBJS),$(obj).cmx)

xs.cma: $(foreach obj,$(OBJS),$(obj).cmo)
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
	ocamlfind install -destdir $(DESTDIR)$(shell ocamlfind printconf destdir) -ldconf ignore xs META $(INTF) xs.mli xst.mli xsraw.mli $(LIBS) *.a *.cmx

.PHONY: uninstall
uninstall:
	ocamlfind remove xs

.PHONY: doc
doc: $(INTF)
	python ../doc/doc.py $(DOCDIR) "xs" "library" "$(OBJS)" "." "xb" ""

clean:
	rm -f *.o *.so *.a *.cmo *.cmi *.cma *.cmx *.cmxa *.annot $(LIBS) $(PROGRAMS)

