IPROG=install -m 755 -o root -g root
CC = gcc
CFLAGS = -Wall -fPIC -O2 -I/opt/xensource/lib/ocaml
OCAMLC = ocamlc -g
OCAMLOPT = ocamlopt

LDFLAGS = -cclib -L./

LIBEXEC  = "/opt/xensource/libexec"
VERSION := $(shell hg parents --template "{rev}" 2>/dev/null || echo 0.0)
OCAMLOPTFLAGS = -g -dtypes

OCAMLABI := $(shell ocamlc -version)
OCAMLLIBDIR := $(shell ocamlc -where)
OCAMLDESTDIR ?= $(OCAMLLIBDIR)


OBJS = 
INTF = $(foreach obj, $(OBJS),$(obj).cmi)
LIBS = 

PROGRAMS = fe

DOCDIR = /myrepos/xen-api-libs.hg/doc

all: $(INTF) $(LIBS) $(PROGRAMS)

bins: $(PROGRAMS)

libs: $(LIBS)

test_forker: test_forker.cmx
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -I ../rpc-light -I ../uuid -I ../stdext uuid.cmxa jsonrpc.cmxa -I ../log unix.cmxa stdext.cmxa  test_forker.cmx -o $@

fe: fe_debug.cmx child.cmx fe_main.cmx
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -I ../rpc-light -I ../stdext -I ../uuid -I ../log log.cmxa uuid.cmxa unix.cmxa jsonrpc.cmxa stdext.cmxa fe_debug.cmx child.cmx fe_main.cmx -o $@ 

%.cmo: %.ml
	$(OCAMLC) -c -I ../log -I ../uuid -I ../stdext -thread -o $@  $<

%.cmi: %.mli
	$(OCAMLC) -c -I ../log -I ../uuid -I ../stdext -o $@  $<

%.cmx: %.ml
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -I ../log -I ../uuid -c -I ../stdext -o $@ $<

META: META.in
	sed 's/@VERSION@/$(VERSION)/g' < $< > $@

.PHONY: install
install: 

.PHONY: bininstall
bininstall: path = $(DESTDIR)$(LIBEXEC)
bininstall: all
	mkdir -p $(path)
	$(IPROG) $(PROGRAMS) $(path)

.PHONY: uninstall
uninstall:

.PHONY: binuninstall
binuninstall:
	rm -f $(DESTDIR)$(LIBEXEC)$(PROGRAMS)

.PHONY: doc
doc: 

clean:
	rm -f *.o *.so *.a *.cmo *.cmi *.cma *.cmx *.cmxa *.annot $(LIBS) $(PROGRAMS)
