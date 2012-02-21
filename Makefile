
idl: types.cmx
	ocamlfind ocamlopt -package xmlm,yojson -linkpkg -o idl types.cmx

%.cmx: %.ml
	ocamlfind ocamlopt -package xmlm,yojson -c $<

.PHONY: clean
clean:
	rm -f *.cmx idl
