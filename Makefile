.PHONY: build release install uninstall clean test doc reindent

build:
	dune build @install

release:
	dune build @install

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
	ocp-indent --syntax cstruct -i **/*.ml*
