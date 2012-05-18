
idl: types.cmx smapiv2.cmx xenops.cmx memory.cmx main.cmx
	ocamlfind ocamlopt -package xmlm,stdext -linkpkg -g -o idl types.cmx smapiv2.cmx xenops.cmx memory.cmx main.cmx

toplevel: types.cmo smapiv2.cmo xenops.cmo memory.cmo
	ocamlfind ocamlmktop -thread -package xmlm,stdext -linkpkg -g -o toplevel types.cmo smapiv2.cmo xenops.cmo memory.cmo

%.cmx: %.ml
	ocamlfind ocamlopt -package xmlm,stdext -c -g $<

%.cmo: %.ml
	ocamlfind ocamlc -package xmlm,stdext -c -g $<

.PHONY: install
install: idl
	./idl
	cp -f test.py doc/test.py
	cp -f xcp.py doc/xcp.py

.PHONY: clean
clean:
	rm -f *.cmx *.cmo idl toplevel
