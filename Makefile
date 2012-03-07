
idl: types.cmx smapiv2.cmx xenops.cmx memory.cmx main.cmx
	ocamlfind ocamlopt -package xmlm,yojson,stdext -linkpkg -g -o idl types.cmx smapiv2.cmx xenops.cmx memory.cmx main.cmx

%.cmx: %.ml
	ocamlfind ocamlopt -package xmlm,yojson,stdext -c -g $<

.PHONY: install
install: idl
	./idl

.PHONY: clean
clean:
	rm -f *.cmx idl
