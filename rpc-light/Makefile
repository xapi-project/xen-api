include ../config.mk

OCAMLFLAGS = -annot -g
PACKS = xmlm

ICAMLP4 = -I $(shell ocamlfind query camlp4) -I $(shell ocamlfind query type-conv)
TARGETS = \
	rpc.cmi rpc.cmo rpc.o rpc.cmx \
	pa_rpc.cma idl.cma \
	xmlrpc.cmi xmlrpc.cmo xmlrpc.o xmlrpc.cmx \
	jsonrpc.cmi jsonrpc.cmo jsonrpc.o jsonrpc.cmx

.PHONY: all clean
all: $(TARGETS)

pa_module_conv.cmo: pa_module_conv.ml
	$(OCAMLC) $(OCAMLFLAGS) -c -package camlp4 -pp "camlp4orf" -I $(shell ocamlfind query camlp4) -o $@ $^

pa_rpc.cma: pa_module_conv.cmo rpc.cmo p4_rpc.cmo pa_rpc.cmo
	$(OCAMLC) -a $(ICAMLP4) -o $@ $^

idl.cma: p4_rpc.cmo p4_idl.cmo 
	$(OCAMLC) -a $(ICAMLP4) -o $@ $^

pa_rpc.cmo: pa_rpc.ml p4_rpc.cmo pa_module_conv.cmo
	$(OCAMLC) $(OCAMLFLAGS) -c -package camlp4,type-conv -pp "camlp4orf" $(ICAMLP4) $@ $<

p4_rpc.cmo: p4_rpc.ml rpc.cmo
	$(OCAMLC) $(OCAMLFLAGS) -c -package camlp4,type-conv -pp "camlp4orf" $(ICAMLP4) $@ $<

p4_idl.cmo: p4_idl.ml p4_rpc.cmo
	$(OCAMLC) $(OCAMLFLAGS) -c -package camlp4,type-conv -pp "camlp4orf" $(ICAMLP4) $@ $<

%.o %.cmx: %.ml
	$(OCAMLOPT) $(OCAMLFLAGS) -package $(PACKS) -c -o $@ $<

%.cmo: %.ml
	$(OCAMLC) $(OCAMLFLAGS) -package $(PACKS) -c -o $@ $<

%.cmi: %.mli %.ml
	$(OCAMLOPT) $(OCAMLFLAGS) -package $(PACKS) -c -o $@ $<

META: META.in
	sed 's/@VERSION@/$(VERSION)/g' < $< > $@

.PHONY: install
install: all META
	ocamlfind install -destdir $(OCAMLDESTDIR) rpc-light META $(TARGETS) $(SPOT)

.PHONY: uninstall
uninstall:
	ocamlfind remove rpc-light

.PHONY: doc
doc: $(INTF)
	python ../doc/doc.py $(DOCDIR) "rpc-light" "package" "jsonrpc pa_rpc rpc xmlrpc" "." "xmlm" ""

clean:
	rm -f *.cmo *.cmx *.cma *.cmxa *.annot $(SPOT) *.o *.cmi *.a
