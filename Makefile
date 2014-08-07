dist/build/lib-rrd/rrd.cmxa:
	obuild configure
	obuild build

.PHONY: install
install:
	ocamlfind install xcp-rrd lib/META $(wildcard dist/build/lib-xcp-rrd/*)

.PHONY: uninstall
uninstall:
	ocamlfind remove xcp-rrd

.PHONY: clean
clean:
	rm -rf dist
