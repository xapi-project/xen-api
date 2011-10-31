include ../config.mk


CFLAGS = -Wall -fPIC -O2 -I$(OCAMLLIBDIR) -D_LARGEFILE_SOURCE -D_LARGEFILE64_SOURCE -D_GNU_SOURCE -fno-strict-aliasing -D_FILE_OFFSET_BITS=64

DESTDIR ?= /
OCAMLFLAGS = -g -dtypes 

OBJS = vhd
INTF = $(foreach obj, $(OBJS),$(obj).cmi)
LIBS = vhd.cma vhd.cmxa


all: $(INTF) $(LIBS) $(PROGRAMS)

bins: $(PROGRAMS)

libs: $(LIBS)

vhd.cmxa: libvhd_stubs.a $(foreach obj,$(OBJS),$(obj).cmx)
	$(OCAMLOPT) $(OCAMLFLAGS) -a -o $@ -cclib -lvhd_stubs -cclib -lvhd $(foreach obj,$(OBJS),$(obj).cmx)

vhd.cma: $(foreach obj,$(OBJS),$(obj).cmo)
	$(OCAMLC) $(OCAMLFLAGS) -a -dllib dllvhd_stubs.so -cclib -lvhd_stubs -cclib -lvhd -o $@ $(foreach obj,$(OBJS),$(obj).cmo)

vhd_stubs.a: vhd_stubs.o
	ocamlmklib -o vhd_stubs -lvhd $+

libvhd_stubs.a: vhd_stubs.o
	ar rcs $@ $+
	ocamlmklib -o vhd_stubs -lvhd $+

%.cmo: %.ml
	$(OCAMLC) $(OCAMLFLAGS) -c -o $@ $<

%.cmi: %.mli
	$(OCAMLC) $(OCAMLFLAGS) -c -o $@ $<

%.cmx: %.ml
	$(OCAMLOPT) $(OCAMLFLAGS) -c -o $@ $<

%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $<

META: META.in
	sed 's/@VERSION@/$(VERSION)/g' < $< > $@

.PHONY: install
install: $(LIBS) META
	ocamlfind install -destdir $(OCAMLDESTDIR) -ldconf ignore vhd META $(INTF) $(LIBS) *.a *.so *.cmx $(SPOT)

.PHONY: uninstall
uninstall:
	ocamlfind remove vhd

.PHONY: doc
doc: $(INTF)
	python ../doc/doc.py $(DOCDIR) "vhd" "package" "$(OBJS)" "." "" ""

clean:
	rm -f META *.o *.so *.a *.cmo *.cmi *.cma *.cmx *.cmxa *.annot $(SPOT) $(LIBS) $(PROGRAMS)
