dist/build/lib-xenops/xenops.cmxa:
	obuild configure
	obuild build

install:
	ocamlfind install xenops src/META $(wildcard dist/build/lib-xenops/*)

uninstall:
	ocamlfind remove xenops

.PHONY: clean
clean:
	rm -rf dist
