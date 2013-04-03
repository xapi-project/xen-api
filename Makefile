dist/build/lib-rrd/rrd.cmxa:
	obuild configure
	obuild build

install:
	ocamlfind install rrd src/META $(wildcard dist/build/lib-rrd/*)

uninstall:
	ocamlfind remove rrd

.PHONY: clean
clean:
	rm -rf dist
