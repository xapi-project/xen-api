.PHONY: build release install uninstall clean test doc reindent

build:
	dune build

release:
	dune build

install:
	dune install

uninstall:
	dune uninstall

clean:
	dune clean

test:
	dune runtest

# requires odoc
doc:
	dune build @doc

reindent:
	git ls-files '*.ml' '*.mli' | xargs ocp-indent -i
