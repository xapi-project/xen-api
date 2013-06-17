.PHONY: clean

dist/build/rrd-protocol/rrd-protocol:
	obuild configure --enable-tests
	obuild build

clean:
	rm -rf dist
