CC = gcc
CFLAGS = -Wall -fPIC -O2 -I/opt/xensource/lib/ocaml
OCAMLC = ocamlc -g
OCAMLOPT = ocamlopt

LDFLAGS = -cclib -L./

DESTDIR ?= /
VERSION := $(shell hg parents --template "{rev}" 2>/dev/null || echo 0.0)
OCAMLOPTFLAGS = -g -dtypes

OCAMLABI := $(shell ocamlc -version)
OCAMLLIBDIR := $(shell ocamlc -where)
OCAMLDESTDIR ?= $(OCAMLLIBDIR)

OCAML_TEST_INC = -I $(shell ocamlfind query oUnit)
OCAML_TEST_LIB = $(shell ocamlfind query oUnit)/oUnit.cmxa

STDEXT_OBJS = filenameext stringext arrayext hashtblext listext pervasiveext threadext ring qring fring opt bigbuffer unixext range vIO trie
INTF = $(foreach obj, $(STDEXT_OBJS),$(obj).cmi)
LIBS = stdext.cma stdext.cmxa

all: $(INTF) $(LIBS) $(PROGRAMS)

bins: $(PROGRAMS)

libs: $(LIBS)

stdext.cmxa: libstdext_stubs.a $(foreach obj,$(STDEXT_OBJS),$(obj).cmx)
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -a -o $@ -cclib -lstdext_stubs $(foreach obj,$(STDEXT_OBJS),$(obj).cmx)

stdext.cma: $(foreach obj,$(STDEXT_OBJS),$(obj).cmo)
	$(OCAMLC) -a -dllib dllstdext_stubs.so -cclib -lstdext_stubs -o $@ $(foreach obj,$(STDEXT_OBJS),$(obj).cmo)

stdext_stubs.a: unixext_stubs.o
	ocamlmklib -o stdext_stubs $+

libstdext_stubs.a: unixext_stubs.o
	ar rcs $@ $+
	ocamlmklib -o stdext_stubs $+

querycd: querycd.cmo 
	 ocamlfind ocamlc -custom -thread -package "threads,unix,stdext" querycd.cmo -o querycd -linkpkg -linkall
#camlc unix.cma stdext.cma querycd.cmo -o querycd

## OBJS
threadext.cmo: threadext.ml
	$(OCAMLC) -thread -c -o $@ $<

%.cmo: %.ml
	$(OCAMLC) -c -I ../uuid -o $@ $<

threadext.cmi: threadext.mli
	$(OCAMLC) -thread -c -o $@ $<

%.cmi: %.mli
	$(OCAMLC) -c -I ../uuid -o $@ $<

threadext.cmx: threadext.ml
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -thread -c -o $@ $<

%.cmx: %.ml
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -I ../uuid -c -o $@ $<

%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $<

META: META.in
	sed 's/@VERSION@/$(VERSION)/g' < $< > $@

.PHONY: install
install: $(LIBS) META
	ocamlfind install -destdir $(DESTDIR)$(shell ocamlfind printconf destdir) -ldconf ignore stdext META $(INTF) $(LIBS) *.a *.so *.cmx

.PHONY: uninstall
uninstall:
	ocamlfind remove stdext

clean:
	rm -f *.o *.so *.a *.cmo *.cmi *.cma *.cmx *.cmxa *.annot $(LIBS) $(PROGRAMS)

#dependency:

unixext.cmo : filenameext.ml
unixext.cmx : filenameext.ml
