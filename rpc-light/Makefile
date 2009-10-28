OCAMLC = ocamlfind ocamlc
OCAMLOPT = ocamlfind ocamlopt
OCAMLFLAGS = -annot -g
PACKS = xmlm

ICAMLP4=-I $(shell ocamlfind query camlp4) -I $(shell ocamlfind query type-conv)

DOCDIR = /myrepos/xen-api-libs.hg/doc

.PHONY: all clean
all: pa_rpc.cma xmlrpc.cmi xmlrpc.cma xmlrpc.cmxa jsonrpc.cmi jsonrpc.cmxa jsonrpc.cma


pa_rpc.cma: rpc.cmo pa_rpc.cmo
	$(OCAMLC) -a $(ICAMLP4) -o $@ $^

pa_rpc.cmo: pa_rpc.ml
	$(OCAMLC) $(OCAMLFLAGS) -c -package camlp4,type-conv -pp "camlp4orf" $(ICAMLP4) $@ $<



rpc.cmx: rpc.ml
	$(OCAMLOPT) $(OCAMLFLAGS) -c -o $@ $<

rpc.cmo: rpc.ml
	$(OCAMLC) $(OCAMLFLAGS) -c -o $@ $<



%.cmxa: rpc.cmx %.cmx
	$(OCAMLOPT) -a -o $@ $^

%.cma: rpc.cmo %.cmo
	$(OCAMLC) -a -o $@ $^



xmlrpc.cmx: xmlrpc.ml xmlrpc.cmi rpc.ml
	$(OCAMLOPT) $(OCAMLFLAGS) -package $(PACKS) -c -o $@ $<

xmlrpc.cmo: xmlrpc.ml xmlrpc.cmi rpc.ml
	$(OCAMLC) $(OCAMLFLAGS) -package $(PACKS) -c -o $@ $<

xmlrpc.cmi: xmlrpc.mli rpc.ml
	$(OCAMLOPT) $(OCAMLFLAGS) -package $(PACKS) -c -o $@ $<


jsonrpc.cmx: jsonrpc.ml jsonrpc.cmi rpc.ml
	$(OCAMLOPT) $(OCAMLFLAGS) -c -o $@ $<

jsonrpc.cmo: jsonrpc.ml jsonrpc.cmi rpc.ml
	$(OCAMLC) $(OCAMLFLAGS) -c  -o $@ $<

jsonrpc.cmi: jsonrpc.mli rpc.ml
	$(OCAMLOPT) $(OCAMLFLAGS) -c -o $@ $<


.PHONY: install
install: rpc.cmi pa_rpc.cma xmlrpc.cma xmlrpc.cmxa
	cp META-xmlrpc META
	ocamlfind install -destdir $(DESTDIR)$(shell ocamlfind printconf destdir) xmlrpc META xmlrpc.cma xmlrpc.cmxa xmlrpc.cmi rpc.cmi xmlrpc.cmx rpc.cmx xmlrpc.a xmlrpc.o
	cp META-jsonrpc META
	ocamlfind install -destdir $(DESTDIR)$(shell ocamlfind printconf destdir) jsonrpc META jsonrpc.cma jsonrpc.cmxa jsonrpc.cmi rpc.cmi jsonrpc.cmx rpc.cmx jsonrpc.a jsonrpc.o
	cp META-rpc-light META
	ocamlfind install -destdir $(DESTDIR)$(shell ocamlfind printconf destdir) rpc-light META pa_rpc.cma pa_rpc.cmi
	rm META

.PHONY: uninstall
uninstall:
	ocamlfind remove xmlrpc
	ocamlfind remove jsonrpc
	ocamlfind remove rpc-light

.PHONY: doc
doc: $(INTF)
	python ../doc/doc.py $(DOCDIR) "rpc-light" "library" "jsonrpc pa_rpc rpc xmlrpc" "." "xmlm" ""
	
clean:
	rm -f *.cmo *.cmx *.cma *.cmxa *.annot *.o *.cmi *.a
