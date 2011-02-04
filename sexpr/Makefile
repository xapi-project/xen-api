include ../config.mk

IPROG=install -m 755
CC = gcc
CFLAGS = -Wall -fPIC -O2 -I/opt/xensource/lib/ocaml
OCAMLC = ocamlc -g
OCAMLOPT = ocamlopt
OCAMLLEX = ocamllex
OCAMLYACC = ocamlyacc

LDFLAGS = -cclib -L./

LIBEXEC  = "/opt/xensource/libexec"
VERSION := $(shell git show-ref --head | grep -E " HEAD$$" | cut -f 1 -d " " || echo 0)
OCAMLOPTFLAGS = -g -dtypes

OCAMLABI := $(shell ocamlc -version)
OCAMLLIBDIR := $(shell ocamlc -where)
OCAMLDESTDIR ?= $(OCAMLLIBDIR)

OBJS = sExpr sExprLexer sExprParser sExpr_TS
INTF = $(foreach obj, $(OBJS),$(obj).cmi)
LIBS = sexpr.cma sexpr.cmxa

PROGRAMS = sexprpp

DOCDIR = ../doc

all: $(INTF) $(LIBS)

bins: $(PROGRAMS)

libs: $(LIBS)

sexprpp: sexprpp.ml all
	ocamlfind $(OCAMLOPT) -I ../stdext stdext.cmxa sexpr.cmxa -linkpkg -o $@ $<

sexpr.cmxa: $(foreach obj,$(OBJS),$(obj).cmx)
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -a -o $@ $(foreach obj,$(OBJS),$(obj).cmx)

sexpr.cma: $(foreach obj,$(OBJS),$(obj).cmo)
	$(OCAMLC) -a -o $@ $(foreach obj,$(OBJS),$(obj).cmo)

sExprLexer.ml: sExprLexer.mll
	$(OCAMLLEX) $<

sExprLexer.mli: sExprParser.cmo sExprLexer.ml
	$(OCAMLC) -i $^ > $@

sExprParser.ml: sExprParser.mly
	$(OCAMLYACC) $<

sExprParser.mli: sExprParser.ml
	$(OCAMLC) -i $< > $@

%.cmo: %.ml %.cmi
	$(OCAMLC) -I ../stdext -c -o $@ $<

%.cmi: %.mli
	$(OCAMLC) -c -o $@ $<

%.cmx: %.ml
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -I ../stdext -c -o $@ $<

%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $<

META: META.in
	sed 's/@VERSION@/$(VERSION)/g' < $< > $@

.PHONY: install
install: path = $(DESTDIR)$(shell ocamlfind printconf destdir)
install: $(LIBS) META
	mkdir -p $(path)
	ocamlfind install -destdir $(path) -ldconf ignore sexpr META $(INTF) $(LIBS) *.a *.cmx *.sp?t

.PHONY: bininstall
bininstall: path = $(DESTDIR)$(LIBEXEC)
bininstall: all
	mkdir -p $(path)
	$(IPROG) $(PROGRAMS) $(path)

.PHONY: uninstall
uninstall:
	ocamlfind remove sexpr

.PHONY: binuninstall
binuninstall:
	rm -f $(DESTDIR)$(LIBEXEC)$(PROGRAMS)

.PHONY: doc
doc: $(INTF)
	python ../doc/doc.py $(DOCDIR) "sexpr" "package" "$(OBJS)" "." "stdext" ""

clean:
	rm -f *.o *.so *.a *.cmo *.cmi *.cma *.cmx *.cmxa *.annot *.sp?t sExprLexer.ml{,i} sExprParser.ml{,i} $(LIBS) $(PROGRAMS)
