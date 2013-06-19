dist/build/lib-xcp-rrd-plugin/xcp-rrd-plugin.cmxa:
	obuild configure
	obuild build

install:
	ocamlfind install xcp-rrd-plugin lib/META $(wildcard dist/build/lib-xcp-rrd-plugin/*)

uninstall:
	ocamlfind remove xcp-rrd-plugin

.PHONY: clean
clean:
	rm -rf dist
