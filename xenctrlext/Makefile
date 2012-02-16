include ../config.mk

IPROG=install -m 755
OCAMLLOC := $(shell ocamlc -where)

OCAML_TEST_INC = -I $(shell ocamlfind query oUnit)
OCAML_TEST_LIB = $(shell ocamlfind query oUnit)/oUnit.cmxa

XENCTRLEXT_OBJS = \
	xenctrlext \

INTF = $(foreach obj, $(XENCTRLEXT_OBJS),$(obj).cmi)
LIBS = xenctrlext.cma xenctrlext.cmxa

PROGRAMS =

all: $(INTF) $(LIBS)

libs: $(LIBS)

xenctrlext.cmxa: libxenctrlext_stubs.a $(foreach obj,$(XENCTRLEXT_OBJS),$(obj).cmx)
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -a -o $@ -cclib -lxenctrlext_stubs $(foreach obj,$(XENCTRLEXT_OBJS),$(obj).cmx)

xenctrlext.cma: $(foreach obj,$(XENCTRLEXT_OBJS),$(obj).cmo)
	$(OCAMLC) -a -dllib dllxenctrlext_stubs.so -cclib -lxenctrlext_stubs -o $@ $(foreach obj,$(XENCTRLEXT_OBJS),$(obj).cmo)

xenctrlext_stubs.a: xenctrlext_stubs.o
	ocamlmklib -o xenctrlext_stubs $+

libxenctrlext_stubs.a: xenctrlext_stubs.o
	ar rcs $@ $+
	ocamlmklib -o xenctrlext_stubs $+

%.cmo: %.ml %.cmi
	$(OCAMLC) -package xenctrl,xenstore -c -o $@ $<

%.cmi: %.mli
	$(OCAMLC) -package xenctrl,xenstore -c -o $@ $<

%.cmx: %.ml %.cmi
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -package xenctrl,xenstore -c -o $@ $<

%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $<

META: META.in
	sed 's/@VERSION@/$(VERSION)/g' < $< > $@

.PHONY: install
install: $(LIBS) META
	mkdir -p $(OCAMLDESTDIR)
	ocamlfind install -destdir $(OCAMLDESTDIR) -ldconf ignore xenctrlext META $(INTF) $(LIBS) *.a *.so *.cmx $(SPOT)

.PHONY: uninstall
uninstall:
	ocamlfind remove xenctrlext

.PHONY: doc
doc: $(INTF)
	python ../doc/doc.py $(DOCDIR) "xenctrlext" "package" "$(XENCTRLEXT_OBJS)" "." "threads,uuid,unix" ""

clean:
	rm -f META *.o *.so *.a *.cmo *.cmi *.cma *.cmx *.cmxa *.annot $(SPOT) $(LIBS) $(PROGRAMS)

