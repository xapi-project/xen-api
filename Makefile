
idl: types.cmx
	ocamlfind ocamlopt -package xmlm,yojson,stdext -linkpkg -o idl types.cmx

%.cmx: %.ml
	ocamlfind ocamlopt -package xmlm,yojson,stdext -c $<

.PHONY: clean
clean:
	rm -f *.cmx idl
