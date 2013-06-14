dist/build/lib-xenops/xenops.cmxa:
	obuild configure
	obuild build

install:
	ocamlfind install xenops lib/META $(wildcard dist/build/lib-xenops/*)

uninstall:
	ocamlfind remove xenopsd

.PHONY: clean
clean:
	rm -rf dist
