dist/build/lib-xcp-rrdd-plugin/xcp-rrdd-plugin.cmxa:
	obuild configure
	obuild build

install:
	ocamlfind install xcp-rrdd-plugin lib/META $(wildcard dist/build/lib-xcp-rrdd-plugin/*)

uninstall:
	ocamlfind remove xcp-rrdd-plugin

.PHONY: clean
clean:
	rm -rf dist
