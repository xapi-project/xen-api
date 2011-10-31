include ../config.mk

IPROG=install -m 755
PACKAGES = xmlm

OBJS = xml
INTF = xml.cmi
LIBS = xml-light2.cma xml-light2.cmxa
PROGRAMS = xmlpp

OCAMLCFLAGS += -package $(PACKAGES)
OCAMLOPTFLAGS += -package $(PACKAGES)

all: $(INTF) $(LIBS)

bins: $(PROGRAMS)

libs: $(LIBS)

xmlpp: xmlpp.ml all
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -I ../stdext stdext.cmxa xml-light2.cmxa -linkpkg -o $@ $<

xml-light2.cmxa: $(foreach obj,$(OBJS),$(obj).cmx)
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -a -o $@ $(foreach obj,$(OBJS),$(obj).cmx)

xml-light2.cma: $(foreach obj,$(OBJS),$(obj).cmo)
	$(OCAMLC) $(OCAMLCFLAGS) -a -o $@ $(foreach obj,$(OBJS),$(obj).cmo)

%.cmo: %.ml
	$(OCAMLC) $(OCAMLCFLAGS) -c -I ../stdext -o $@ $<

%.cmi: %.mli
	$(OCAMLC) $(OCAMLCFLAGS) -c -I ../stdext -o $@ $<

%.cmx: %.ml
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c -I ../stdext -o $@ $<

%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $<

META: META.in
	sed 's/@VERSION@/$(VERSION)/g' < $< > $@

.PHONY: install
install: $(LIBS) META
	mkdir -p $(OCAMLDESTDIR)
	ocamlfind install -destdir $(OCAMLDESTDIR) -ldconf ignore xml-light2 META $(INTF) $(LIBS) *.a *.cmx $(SPOT)

.PHONY: bininstall
bininstall: path = $(DESTDIR)$(LIBEXEC)
bininstall: all
	mkdir -p $(path)
	$(IPROG) $(PROGRAMS) $(path)

.PHONY: uninstall
uninstall:
	ocamlfind remove xml-light2

.PHONY: binuninstall
binuninstall:
	rm -f $(DESTDIR)$(LIBEXEC)$(PROGRAMS)

.PHONY: doc
doc: $(INTF)
	python ../doc/doc.py $(DOCDIR) "xml-light2" "package" "$(OBJS)" "." "stdext,xmlm" ""

clean:
	rm -f META *.o *.so *.a *.cmo *.cmi *.cma *.cmx *.cmxa *.annot $(SPOT) $(LIBS) $(PROGRAMS)

