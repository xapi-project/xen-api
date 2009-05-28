CC = gcc
CFLAGS = -Wall -fPIC -O2 -I/opt/xensource/lib/ocaml -I$(XEN_ROOT)/usr/include -I../mmap -I./
OCAMLC = ocamlc -g
OCAMLOPT = ocamlopt
OCAMLOPTFLAGS = -g -dtypes -I ./

LDFLAGS = -cclib -L./

DESTDIR ?= /
VERSION := $(shell hg parents --template "{rev}" 2>/dev/null || echo 0.0)

OCAMLABI := $(shell ocamlc -version)
OCAMLLIBDIR := $(shell ocamlc -where)
OCAMLDESTDIR ?= $(OCAMLLIBDIR)

OBJS = sha1
INTF = sha1.cmi
LIBS = sha1.cma sha1.cmxa

PROGRAMS =

all: $(INTF) $(LIBS) $(PROGRAMS)

bins: $(PROGRAMS)

libs: $(LIBS)

sha1.cmxa: libsha1_stubs.a $(foreach obj,$(OBJS),$(obj).cmx)
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -a -o $@ -cclib -lsha1_stubs $(foreach obj,$(OBJS),$(obj).cmx)

sha1.cma: $(foreach obj,$(OBJS),$(obj).cmo)
	$(OCAMLC) -a -dllib dllsha1_stubs.so -cclib -lsha1_stubs -o $@ $(foreach obj,$(OBJS),$(obj).cmo)

sha1_stubs.a: sha1_stubs.o
	ocamlmklib -o sha1_stubs $+

libsha1_stubs.a: sha1_stubs.o
	ar rcs $@ $+
	ocamlmklib -o sha1_stubs $+

%.cmo: %.ml
	$(OCAMLC) -c -o $@ $<

%.cmi: %.mli
	$(OCAMLC) -c -o $@ $<

%.mli: %.ml
	$(OCAMLC) -i $< > $@

%.cmx: %.ml
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c -o $@ $<

%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $<

META: META.in
	sed 's/@VERSION@/$(VERSION)/g' < $< > $@

.PHONY: install
install: $(LIBS) META
	ocamlfind install -destdir $(DESTDIR)$(shell ocamlfind printconf destdir) -ldconf ignore sha1 META $(INTF) $(LIBS) *.a *.so *.cmx

.PHONY: uninstall
uninstall:
	ocamlfind remove sha1

clean:
	rm -f *.o *.so *.a *.cmo *.cmi *.cma *.cmx *.cmxa *.annot $(LIBS) $(PROGRAMS) $(INTF)

