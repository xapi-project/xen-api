include ../config.mk

IPROG=install -m 755
OCAMLLEX = ocamllex

OBJS = sExpr sExprLexer sExprParser sExpr_TS
INTF = $(foreach obj, $(OBJS),$(obj).cmi)
LIBS = sexpr.cma sexpr.cmxa

PROGRAMS = sexprpp

all: $(INTF) $(LIBS)

bins: $(PROGRAMS)

libs: $(LIBS)

sexprpp: sexprpp.ml all
	$(OCAMLOPT) -I ../stdext stdext.cmxa sexpr.cmxa -linkpkg -o $@ $<

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
install: $(LIBS) META
	mkdir -p $(OCAMLDESTDIR)
	ocamlfind install -destdir $(OCAMLDESTDIR) -ldconf ignore sexpr META $(INTF) $(LIBS) *.a *.cmx $(SPOT)

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
	rm -f META *.o *.so *.a *.cmo *.cmi *.cma *.cmx *.cmxa *.annot $(SPOT) sExprLexer.ml{,i} sExprParser.ml{,i} $(LIBS) $(PROGRAMS)
