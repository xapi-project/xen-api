OCAMLC = ocamlfind ocamlc
OCAMLOPT = ocamlfind ocamlopt
OCAMLFLAGS = -annot -g
PACKS = xmlm

ICAMLP4 = -I $(shell ocamlfind query camlp4) -I $(shell ocamlfind query type-conv)
DOCDIR  = /myrepos/xen-api-libs/doc
TARGETS = \
	rpc.cmi rpc.cmo rpc.o rpc.cmx \
	pa_rpc.cma \
	xmlrpc.cmi xmlrpc.cmo xmlrpc.o xmlrpc.cmx \
	jsonrpc.cmi jsonrpc.cmo jsonrpc.o jsonrpc.cmx

.PHONY: all clean
all: $(TARGETS)

pa_rpc.cma: rpc.cmo p4_rpc.cmo pa_rpc.cmo
	$(OCAMLC) -a $(ICAMLP4) -o $@ $^

pa_rpc.cmo: pa_rpc.ml p4_rpc.cmo
	$(OCAMLC) $(OCAMLFLAGS) -c -package camlp4,type-conv -pp "camlp4orf" $(ICAMLP4) $@ $<

p4_rpc.cmo: p4_rpc.ml rpc.cmo
	$(OCAMLC) $(OCAMLFLAGS) -c -package camlp4,type-conv -pp "camlp4orf" $(ICAMLP4) $@ $<

%.o %.cmx: %.ml
	$(OCAMLOPT) $(OCAMLFLAGS) -package $(PACKS) -c -o $@ $<

%.cmo: %.ml
	$(OCAMLC) $(OCAMLFLAGS) -package $(PACKS) -c -o $@ $<

%.cmi: %.mli %.ml
	$(OCAMLOPT) $(OCAMLFLAGS) -package $(PACKS) -c -o $@ $<

.PHONY: install
install: INSTALL_PATH = $(DESTDIR)$(shell ocamlfind printconf destdir)
install: all
	ocamlfind install -destdir $(INSTALL_PATH) rpc-light META $(TARGETS)

.PHONY: uninstall
uninstall:
	ocamlfind remove rpc-light

.PHONY: doc
doc: $(INTF)
	python ../doc/doc.py $(DOCDIR) "rpc-light" "package" "jsonrpc pa_rpc rpc xmlrpc" "." "xmlm" ""

clean:
	rm -f *.cmo *.cmx *.cma *.cmxa *.annot *.o *.cmi *.a
