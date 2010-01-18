OCAMLC = ocamlfind ocamlc
OCAMLOPT = ocamlfind ocamlopt
OCAMLFLAGS = -annot -g

PACKS = rpc-light
EXAMPLES = \
	all_types \
	phantom \
	xapi \
	encoding \
	dict \
	variants

EXECS=$(foreach example, $(EXAMPLES), $(example).opt)

.PHONY: all clean
all: $(EXECS)

%.opt: %.cmx
	$(OCAMLOPT) -linkpkg -package $(PACKS) -o $@ $<

%.cmx: %.ml
	$(OCAMLOPT) $(OCAMLFLAGS) -package $(PACKS),rpc-light.syntax -syntax camlp4o -c -o $@ $<

%_gen: %.ml
	camlp4o $(shell ocamlfind query rpc-light.syntax -r -format "-I %d %a" -predicates syntax,preprocessor) $< -printer o > $@.ml
	$(OCAMLOPT) $(OCAMLFLAGS) -package $(PACKS) -c -o $@ $@.ml

clean:
	rm -f *.cmx *.cmi *.cmo *.cmxa *.o $(EXECS)
