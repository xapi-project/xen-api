include ../config.mk

IPROG=install -m 755
OCAMLLOC := $(shell ocamlc -where)

OCAML_TEST_INC = -I $(shell ocamlfind query oUnit)
OCAML_TEST_LIB = $(shell ocamlfind query oUnit)/oUnit.cmxa

XENSTOREEXT_OBJS = \
	watch

INTF = $(foreach obj, $(XENSTOREEXT_OBJS),$(obj).cmi)
LIBS = xenstoreext.cma xenstoreext.cmxa

PROGRAMS =

all: $(INTF) $(LIBS)

libs: $(LIBS)

xenstoreext.cmxa: $(foreach obj,$(XENSTOREEXT_OBJS),$(obj).cmx)
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -a -o $@ $(foreach obj,$(XENSTOREEXT_OBJS),$(obj).cmx)

xenstoreext.cma: $(foreach obj,$(XENSTOREEXT_OBJS),$(obj).cmo)
	$(OCAMLC) -a -o $@ $(foreach obj,$(XENSTOREEXT_OBJS),$(obj).cmo)

%.cmo: %.ml %.cmi
	$(OCAMLC) -package xenctrl,xenstore -c -I ../log -I ../stdext -o $@ $<

%.cmi: %.mli
	$(OCAMLC) -package xenctrl,xenstore -c -I ../log -I ../stdext -o $@ $<

%.cmx: %.ml %.cmi
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -package xenctrl,xenstore -c -I ../log -I ../stdext -o $@ $<

%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $<

META: META.in
	sed 's/@VERSION@/$(VERSION)/g' < $< > $@

.PHONY: install
install: $(LIBS) META
	mkdir -p $(OCAMLDESTDIR)
	ocamlfind install -destdir $(OCAMLDESTDIR) -ldconf ignore xenstoreext META $(INTF) $(LIBS) *.a *.cmx $(SPOT)

.PHONY: uninstall
uninstall:
	ocamlfind remove xenstoreext

.PHONY: doc
doc: $(INTF)
	python ../doc/doc.py $(DOCDIR) "xenstoreext" "package" "$(XENSTOREEXT_OBJS)" "." "threads,uuid,unix" ""

clean:
	rm -f META *.o *.so *.a *.cmo *.cmi *.cma *.cmx *.cmxa *.annot $(SPOT) $(LIBS) $(PROGRAMS)

