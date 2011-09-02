include ../config.mk

CC = gcc
CFLAGS = -Wall -fPIC -O2 -I/opt/xensource/lib/ocaml
OCAMLC = ocamlc -g
OCAMLOPT = ocamlopt
OCAMLFIND = ocamlfind

LDFLAGS = -cclib -L./

VERSION := $(shell git show-ref --head | grep -E " HEAD$$" | cut -f 1 -d " " || echo 0)
OCAMLOPTFLAGS = -g -dtypes

PP = camlp4o -I ../rpc-light -I $(shell ocamlfind query type-conv) pa_type_conv.cmo pa_rpc.cma

OCAMLABI := $(shell ocamlc -version)
OCAMLLIBDIR := $(shell ocamlc -where)
OCAMLDESTDIR ?= $(OCAMLLIBDIR)

OBJS = radix_tree server_io buf_io http http_svr http_client test_server test_client
INTF = $(foreach obj, $(OBJS),$(obj).cmi)
LIBS = http_svr.cma http_svr.cmxa
PROGRAMS = test_server radix_tree


DOCDIR = ../doc

all: $(INTF) $(LIBS)

bins: $(PROGRAMS)

libs: $(LIBS)

radix_tree.opt: radix_tree.cmx radix_tree_test.cmx
	$(OCAMLFIND) $(OCAMLOPT) $(OCAMLOPTFLAGS) -package "stdext,unix" -linkpkg -o $@ radix_tree.cmx radix_tree_test.cmx

test_server.opt: http_svr.cmxa test_server.cmx
	$(OCAMLFIND) $(OCAMLOPT) $(OCAMLOPTFLAGS) -package "stdext,unix,threads" -thread -linkpkg -o $@ -cclib -L../log ../log/log.cmxa http_svr.cmxa test_server.cmx

test_client.opt: http_svr.cmxa test_client.cmx
	$(OCAMLFIND) $(OCAMLOPT) $(OCAMLOPTFLAGS) -package "stdext,unix,threads" -thread -linkpkg -o $@ -cclib -L../log ../log/log.cmxa ../stunnel/stunnel.cmxa http_svr.cmxa test_client.cmx

http_svr.cmxa: $(foreach obj,$(OBJS),$(obj).cmx)
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -a -o $@ $(foreach obj,$(OBJS),$(obj).cmx)

http_svr.cma: $(foreach obj,$(OBJS),$(obj).cmo)
	$(OCAMLC) -a -o $@ $(foreach obj,$(OBJS),$(obj).cmo)

%.cmo: %.ml %.cmi
	$(OCAMLC) -c -pp '${PP}' -thread -I ../rpc-light -I ../stdext -I ../log -I ../stunnel -o $@ $<

%.cmi: %.mli
	$(OCAMLC) -c -I ../rpc-light -thread -o $@ $<

%.cmx: %.ml %.cmi
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -pp '${PP}' -c -thread -I ../rpc-light -I ../stdext -I ../log -I ../stunnel -o $@ $<

%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $<

META: META.in
	sed 's/@VERSION@/$(VERSION)/g' < $< > $@

.PHONY: install
install: path = $(DESTDIR)$(shell ocamlfind printconf destdir)
install: $(LIBS) META
	mkdir -p $(path)
	ocamlfind install -destdir $(path) -ldconf ignore http-svr META $(INTF) $(LIBS) *.a *.cmx *.sp?t

.PHONY: uninstall
uninstall:
	ocamlfind remove http-svr

.PHONY: doc
doc: $(INTF)
	python ../doc/doc.py $(DOCDIR) "http-svr" "package" "$(OBJS)" "." "log,stdext" ""

clean:
	rm -f *.o *.so *.a *.cmo *.cmi *.cma *.cmx *.cmxa *.annot *.sp?t $(LIBS) $(PROGRAMS)
