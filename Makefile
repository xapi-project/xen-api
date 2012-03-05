
idl: types.cmx smapiv2.cmx xenops.cmx main.cmx
	ocamlfind ocamlopt -package xmlm,yojson,stdext -linkpkg -o idl types.cmx smapiv2.cmx xenops.cmx main.cmx

%.cmx: %.ml
	ocamlfind ocamlopt -package xmlm,yojson,stdext -c $<

.PHONY: install
install: idl
	./idl

.PHONY: clean
clean:
	rm -f *.cmx idl
