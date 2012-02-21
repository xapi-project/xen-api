
idl: types.cmx
	ocamlfind ocamlopt -package xmlm -linkpkg -o idl types.cmx

%.cmx: %.ml
	ocamlfind ocamlopt -package xmlm -c $<

.PHONY: clean
clean:
	rm -f *.cmx idl
