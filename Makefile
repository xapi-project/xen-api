
idl: types.cmx smapiv2.cmx xenops.cmx main.cmx
	ocamlfind ocamlopt -package xmlm,yojson,stdext -linkpkg -o idl types.cmx smapiv2.cmx xenops.cmx main.cmx

%.cmx: %.ml
	ocamlfind ocamlopt -package xmlm,yojson,stdext -c $<

.PHONY: clean
clean:
	rm -f *.cmx idl
