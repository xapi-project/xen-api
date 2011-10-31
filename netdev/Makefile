include ../config.mk

OCAMLFLAGS = -I ../stdext

OBJS = netdev
INTF = $(foreach obj, $(OBJS),$(obj).cmi)
LIBS = netdev.cma netdev.cmxa

all: $(INTF) $(LIBS) $(PROGRAMS)

bins: $(PROGRAMS)

libs: $(LIBS)

netdev.cmxa: libnetdev_stubs.a $(foreach obj,$(OBJS),$(obj).cmx)
	$(OCAMLOPT) $(OCAMLOPTFLAGS) $(OCAMLFLAGS) -a -o $@ -cclib -lnetdev_stubs $(foreach obj,$(OBJS),$(obj).cmx)

netdev.cma: libnetdev_stubs.a $(foreach obj,$(OBJS),$(obj).cmo)
	$(OCAMLC) $(OCAMLFLAGS) -a -dllib dllnetdev_stubs.so -cclib -lnetdev_stubs -o $@ $(foreach obj,$(OBJS),$(obj).cmo)

netdev_stubs.a: addr_stubs.o bridge_stubs.o link_stubs.o
	ocamlmklib -o netdev_stubs $+

libnetdev_stubs.a: addr_stubs.o bridge_stubs.o link_stubs.o
	ar rcs $@ $+
	ocamlmklib -o netdev_stubs $+

%.cmo: %.ml
	$(OCAMLC) $(OCAMLFLAGS) -c -o $@ $<

%.cmi: %.mli
	$(OCAMLC) $(OCAMLFLAGS) -c -o $@ $<

%.cmx: %.ml
	$(OCAMLOPT) $(OCAMLFLAGS) $(OCAMLOPTFLAGS) -c -o $@ $<

%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $<

META: META.in
	sed 's/@VERSION@/$(VERSION)/g' < $< > $@

.PHONY: install
install: $(LIBS) META
	mkdir -p $(OCAMLDESTDIR)
	ocamlfind install -destdir $(OCAMLDESTDIR) -ldconf ignore netdev META $(INTF) $(LIBS) *.a *.so *.cmx $(SPOT)

.PHONY: uninstall
uninstall:
	ocamlfind remove netdev 

.PHONY: doc
doc: $(INTF)
	python ../doc/doc.py $(DOCDIR) "netdev" "package" "$(OBJS)" "." "" ""

clean:
	rm -f META *.o *.so *.a *.cmo *.cmi *.cma *.cmx *.cmxa *.annot $(SPOT) $(LIBS) $(PROGRAMS)

