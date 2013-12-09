.PHONY: clean install uninstall

dist/build/lib-rrd_transport/rrd-transport.cma:
	obuild configure --enable-tests
	obuild build

install:
	ocamlfind install rrd-transport lib/META $(wildcard dist/build/lib-rrd_transport/*)

uninstall:
	ocamlfind remove rrd-transport

clean:
	rm -rf dist
