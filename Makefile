dist/build/lib-xcp-inventory/xcp-inventory.cmxa: lib/xcp_inventory_config.ml
	obuild configure
	obuild build

lib/xcp_inventory_config.ml:
	@echo "You need to run configure first"
	@exit 1

install:
	ocamlfind install xcp-inventory lib/META $(wildcard dist/build/lib-xcp-inventory/*)

uninstall:
	ocamlfind remove xcp-inventory

real-configure: configure.ml
	ocamlfind ocamlc -linkpkg -package findlib,cmdliner -o real-configure configure.ml
	@rm -f configure.cm*

.PHONY: clean distclean
clean:
	rm -rf dist

distclean: clean
	rm -f lib/xcp_inventory_config.ml
