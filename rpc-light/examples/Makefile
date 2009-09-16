OCAMLC = ocamlfind ocamlc
OCAMLOPT = ocamlfind ocamlopt
OCAMLFLAGS = -annot

PACKS = xmlrpc
EXAMPLES = all_types

EXECS=$(foreach example, $(EXAMPLES), $(example).opt)

.PHONY: all clean
all: $(EXECS)

%.opt: %.cmx
	$(OCAMLOPT) -linkpkg -package $(PACKS) -o $@ $<

%.cmx: %.ml
	$(OCAMLOPT) -package $(PACKS),rpc-light.syntax -syntax camlp4o -c -o $@ $<

clean:
	rm -f *.cmx *.cmi *.cmo *.cmxa *.o $(EXECS)