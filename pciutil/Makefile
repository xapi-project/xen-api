include ../config.mk

IPROG=install -m 755


OBJS = pciutil
INTF = $(foreach obj, $(OBJS),$(obj).cmi)
LIBS = pciutil.cma pciutil.cmxa

PROGRAMS = pciutil

all: $(INTF) $(LIBS)

bins: $(PROGRAMS)

libs: $(LIBS)

pciutil: pciutil_main.cmx
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -I ./ -I ../stdext -I ../uuid unix.cmxa uuid.cmxa stdext.cmxa pciutil.cmx -o $@ $^

pciutil.cmxa: $(foreach obj,$(OBJS),$(obj).cmx)
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -a -o $@ $(foreach obj,$(OBJS),$(obj).cmx)

pciutil.cma: $(foreach obj,$(OBJS),$(obj).cmo)
	$(OCAMLC) -a -o $@ $(foreach obj,$(OBJS),$(obj).cmo)

%.cmo: %.ml
	$(OCAMLC) -c -I ../stdext -o $@ $<

%.cmi: %.mli
	$(OCAMLC) -c -I ../stdext -o $@ $<

%.cmx: %.ml
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c -I ../stdext -o $@ $<

%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $<

META: META.in
	sed 's/@VERSION@/$(VERSION)/g' < $< > $@

.PHONY: install
install: $(LIBS) META
	mkdir -p $(OCAMLDESTDIR)
	ocamlfind install -destdir $(OCAMLDESTDIR) -ldconf ignore pciutil META $(INTF) $(LIBS) *.a *.cmx $(SPOT)

.PHONY: bininstall
bininstall: path = $(DESTDIR)$(LIBEXEC)
bininstall: all
	mkdir -p $(path)
	$(IPROG) $(PROGRAMS) $(path)

.PHONY: uninstall
uninstall:
	ocamlfind remove pciutil

.PHONY: binuninstall
binuninstall:
	rm -f $(DESTDIR)$(LIBEXEC)$(PROGRAMS)

.PHONY: doc
doc: $(INTF)
	python ../doc/doc.py $(DOCDIR) "pciutil" "package" "$(OBJS)" "." "stdext" ""

clean:
	rm -f META *.o *.so *.a *.cmo *.cmi *.cma *.cmx *.cmxa *.annot $(SPOT) $(LIBS) $(PROGRAMS)
