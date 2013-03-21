dist/build/lib-xcp-inventory/xcp-inventory.cmxa:
	obuild configure
	obuild build

install:
	ocamlfind install xcp-inventory lib/META $(wildcard dist/build/lib-xcp-inventory/*)

uninstall:
	ocamlfind remove xcp-inventory

.PHONY: clean
clean:
	rm -rf dist
