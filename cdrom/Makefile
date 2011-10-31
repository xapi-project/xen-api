include ../config.mk

OBJS = cdrom
INTF = $(foreach obj, $(OBJS),$(obj).cmi)
LIBS = cdrom.cma cdrom.cmxa

all: $(INTF) $(LIBS) $(PROGRAMS)

bins: $(PROGRAMS)

libs: $(LIBS)

cdrom.cmxa: libcdrom_stubs.a $(foreach obj,$(OBJS),$(obj).cmx)
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -a -o $@ -cclib -lcdrom_stubs $(foreach obj,$(OBJS),$(obj).cmx)

cdrom.cma: $(foreach obj,$(OBJS),$(obj).cmo)
	$(OCAMLC) -a -dllib dllcdrom_stubs.so -cclib -lcdrom_stubs -o $@ $(foreach obj,$(OBJS),$(obj).cmo)

cdrom_stubs.a: cdrom_stubs.o
	ocamlmklib -o cdrom_stubs $+

libcdrom_stubs.a: cdrom_stubs.o
	ar rcs $@ $+
	ocamlmklib -o cdrom_stubs $+

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
	mkdir -p $(OCAMLDESTDIR)
	ocamlfind install -destdir $(OCAMLDESTDIR) -ldconf ignore cdrom META $(INTF) $(LIBS) *.a *.so *.cmx $(SPOT)

.PHONY: uninstall
uninstall:
	ocamlfind remove cdrom

.PHONY: doc
doc: $(INTF)
	python ../doc/doc.py $(DOCDIR) "cdrom" "package" "$(OBJS)" "." "" ""

clean:
	rm -f META *.o *.so *.a *.cmo *.cmi *.cma *.cmx *.cmxa *.annot $(SPOT) $(LIBS) $(PROGRAMS)
