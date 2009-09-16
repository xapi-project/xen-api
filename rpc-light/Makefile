OCAMLC = ocamlfind ocamlc
OCAMLOPT = ocamlfind ocamlopt
OCAMLFLAGS = -annot

ICAMLP4=-I $(shell ocamlfind query camlp4) -I $(shell ocamlfind query type-conv)

.PHONY: all clean
all: pa_rpc.cma xmlrpc.cma xmlrpc.cmxa

pa_rpc.cma: rpc.cmo pa_rpc.cmo
	$(OCAMLC) -a $(ICAMLP4) -o $@ $^

xmlrpc.cmxa: rpc.cmx xmlrpc.cmx
	$(OCAMLOPT) -a -o $@ $^

xmlrpc.cma: rpc.cmo xmlrpc.cmo
	$(OCAMLC) -a -o $@ $^


xmlrpc.cmx: xmlrpc.ml
	$(OCAMLOPT) $(OCAMLFLAGS) -c -I ../xml-light2 -o $@ $<

xmlrpc.cmo: xmlrpc.ml
	$(OCAMLC) $(OCAMLFLAGES) -c -I ../xml-light2 -o $@ $<


rpc.cmx: rpc.ml
	$(OCAMLOPT) $(OCAMLFLAGS) -c -o $@ $<

rpc.cmo: rpc.ml
	$(OCAMLC) $(OCAMLFLAGS) -c -o $@ $<


pa_rpc.cmo: pa_rpc.ml
	$(OCAMLC) $(OCAMLFLAGS) -c -package camlp4,type-conv -pp "camlp4orf" $(ICAMLP4) $@ $<


.PHONY: install
install: rpc.cmi pa_rpc.cma xmlrpc.cma xmlrpc.cmxa
	cp META-xmlrpc META
	ocamlfind install -destdir $(DESTDIR)$(shell ocamlfind printconf destdir) xmlrpc META xmlrpc.cma xmlrpc.cmxa xmlrpc.cmi rpc.cmi xmlrpc.cmx rpc.cmx xmlrpc.a xmlrpc.o
	cp META-rpc-light META
	ocamlfind install -destdir $(DESTDIR)$(shell ocamlfind printconf destdir) rpc-light META pa_rpc.cma pa_rpc.cmi
	rm META

.PHONY: uninstall
uninstall:
	ocamlfind remove xmlrpc
	ocamlfind remove rpc-light

clean:
	rm -f *.cmo *.cmx *.cma *.cmxa *.annot *.o *.cmi *.a