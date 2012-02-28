include ../config.mk

OBJS = nbd
INTF = $(foreach obj, $(OBJS),$(obj).cmi)
LIBS = nbd.cma nbd.cmxa
FLAGS = -package bitstring.syntax -syntax camlp4o -I ../stdext
all: $(INTF) $(LIBS) $(PROGRAMS)

bins: $(PROGRAMS)

libs: $(LIBS)

nbd.cmxa: $(foreach obj,$(OBJS),$(obj).cmx)
	$(OCAMLOPT) $(FLAGS) -a -o $@ $(foreach obj,$(OBJS),$(obj).cmx)

nbd.cma: $(foreach obj,$(OBJS),$(obj).cmo)
	$(OCAMLC) $(FLAGS) -a -o $@ $(foreach obj,$(OBJS),$(obj).cmo)

%.cmo: %.ml
	$(OCAMLFIND) $(OCAMLC) -c $(FLAGS) -o $@ $<

%.cmi: %.mli
	$(OCAMLC) -c $(FLAGS) -o $@ $<

%.cmx: %.ml
	$(OCAMLOPT) $(FLAGS) -c -o $@ $<

%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $<

META: META.in
	sed 's/@VERSION@/$(VERSION)/g' < $< > $@

.PHONY: install
install: $(LIBS) META
	mkdir -p $(OCAMLDESTDIR)
	ocamlfind install -destdir $(OCAMLDESTDIR) -ldconf ignore nbd META $(INTF) $(LIBS) *.a *.cmx $(SPOT)

.PHONY: uninstall
uninstall:
	ocamlfind remove nbd

.PHONY: doc
doc: $(INTF)
	python ../doc/doc.py $(DOCDIR) "nbd" "package" "$(OBJS)" "." ""

clean:
	rm -f META *.o *.so *.a *.cmo *.cmi *.cma *.cmx *.cmxa *.annot $(SPOT) $(LIBS) $(PROGRAMS)
