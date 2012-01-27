include ../config.mk

INCLUDES = -I ../stdext -I ../xenctrlext $(OCAML_XC_INC) -I ../xen-utils

OBJS = cpuid
INTF = $(foreach obj, $(OBJS),$(obj).cmi)
LIBS = cpuid.cma cpuid.cmxa

all: $(INTF) $(LIBS) $(PROGRAMS)

bins: $(PROGRAMS)

libs: $(LIBS)

cpuid.cmxa: cpuid_stubs.a $(foreach obj,$(OBJS),$(obj).cmx)
	$(OCAMLOPT) $(OCAMLOPTFLAGS) $(INCLUDES) -a -o $@ -cclib -lcpuid_stubs $(foreach obj,$(OBJS),$(obj).cmx)

cpuid.cma: $(foreach obj,$(OBJS),$(obj).cmo)
	$(OCAMLC) -a -dllib dllcpuid_stubs.so -cclib -lcpuid_stubs $(INCLUDES) -o $@ $(foreach obj,$(OBJS),$(obj).cmo)

cpuid_stubs.a: cpuid_stubs.o
	ocamlmklib -o cpuid_stubs $+

libcpuid_stubs.a: cpuid_stubs.o
	ar rcs $@ $+
	ocamlmklib -o cpuid_stubs $+

%.cmo: %.ml
	$(OCAMLC) $(INCLUDES) -c -o $@ $<

%.cmi: %.mli
	$(OCAMLC) $(INCLUDES) -c -o $@ $<

%.cmx: %.ml
	$(OCAMLOPT) $(OCAMLOPTFLAGS) $(INCLUDES) -c -o $@ $<

%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $<

META: META.in
	sed 's/@VERSION@/$(VERSION)/g' < $< > $@

.PHONY: install
install: $(LIBS) META
	mkdir -p $(OCAMLDESTDIR)
	ocamlfind install -destdir $(OCAMLDESTDIR) -ldconf ignore cpuid META $(INTF) $(LIBS) *.a *.so *.cmx $(SPOT)

.PHONY: uninstall
uninstall:
	ocamlfind remove cpuid

.PHONY: doc
doc: $(INTF)
	python ../doc/doc.py $(DOCDIR) "cpuid" "package" "$(OBJS)" "." "stdext xenctrl xenctrlext xen-utils" ""

clean:
	rm -f META *.o *.so *.a *.cmo *.cmi *.cma *.cmx *.cmxa *.annot $(SPOT) $(LIBS) $(PROGRAMS) *~ *.rej

