dist/build/lib-rrd/rrd.cmxa:
	obuild configure
	obuild build

install:
	ocamlfind install xcp-rrd lib/META $(wildcard dist/build/lib-xcp-rrd/*)

uninstall:
	ocamlfind remove xcp-rrd

.PHONY: clean
clean:
	rm -rf dist
