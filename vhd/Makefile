include ../config.mk


OCAMLFIND = ocamlfind


DESTDIR ?= /
OCAMLFLAGS = -g -dtypes 

OBJS = vhd
INTF = $(foreach obj, $(OBJS),$(obj).cmi)
LIBS = vhd.cma vhd.cmxa


all: $(INTF) $(LIBS) $(PROGRAMS)

bins: $(PROGRAMS)

libs: $(LIBS)

vhd.cmxa: libvhd_stubs.a $(foreach obj,$(OBJS),$(obj).cmx)
	$(OCAMLFIND) $(OCAMLOPT) $(OCAMLFLAGS) -a -o $@ -cclib -lvhd_stubs -cclib -lvhd $(foreach obj,$(OBJS),$(obj).cmx)

vhd.cma: $(foreach obj,$(OBJS),$(obj).cmo)
	$(OCAMLFIND) $(OCAMLC) $(OCAMLFLAGS) -a -dllib dllvhd_stubs.so -cclib -lvhd_stubs -cclib -lvhd -o $@ $(foreach obj,$(OBJS),$(obj).cmo)

vhd_stubs.a: vhd_stubs.o
	ocamlmklib -o vhd_stubs -lvhd $+

libvhd_stubs.a: vhd_stubs.o
	ar rcs $@ $+
	ocamlmklib -o vhd_stubs -lvhd $+

%.cmo: %.ml
	$(OCAMLFIND) $(OCAMLC) $(OCAMLFLAGS) -c -o $@ $<

%.cmi: %.mli
	$(OCAMLFIND) $(OCAMLC) $(OCAMLFLAGS) -c -o $@ $<

%.cmx: %.ml
	$(OCAMLFIND) $(OCAMLOPT) $(OCAMLFLAGS) -c -o $@ $<

%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $<

META: META.in
	sed 's/@VERSION@/$(VERSION)/g' < $< > $@

.PHONY: install
install: $(LIBS) META
	ocamlfind install -destdir $(DESTDIR)$(shell ocamlfind printconf destdir) -ldconf ignore vhd META $(INTF) $(LIBS) *.a *.so *.cmx *.sp?t

.PHONY: uninstall
uninstall:
	ocamlfind remove vhd

.PHONY: doc
doc: $(INTF)
	python ../doc/doc.py $(DOCDIR) "vhd" "package" "$(OBJS)" "." "" ""

clean:
	rm -f *.o *.so *.a *.cmo *.cmi *.cma *.cmx *.cmxa *.annot *.sp?t $(LIBS) $(PROGRAMS)
