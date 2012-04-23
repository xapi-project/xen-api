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

testidl_test: testidl.ml testidl_test.ml
	ocamlc -pp 'camlp4o /usr/lib/ocaml/type-conv/pa_type_conv.cmo ../p4_rpc.cmo ../p4_idl.cmo' -I .. rpc.cmo testidl.ml
	ocamlc -I .. ../rpc.cmo testidl.cmo testidl_test.ml -o testidl_test

%.opt: %.cmx
	$(OCAMLOPT) -linkpkg -package $(PACKS) -o $@ $<

%.cmx: %.ml
	$(OCAMLOPT) $(OCAMLFLAGS) -package $(PACKS),rpc-light.syntax -syntax camlp4o -c -o $@ $<

%_gen: %.ml
	camlp4o $(shell ocamlfind query rpc-light.syntax -r -format "-I %d %a" -predicates syntax,preprocessor) $< -printer o > $@.ml
	$(OCAMLOPT) $(OCAMLFLAGS) -package $(PACKS) -c -o $@ $@.ml

clean:
	rm -f *.cmx *.cmi *.cmo *.cmxa *.o $(EXECS)
