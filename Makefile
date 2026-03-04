.PHONY: build release install uninstall clean test doc reindent

build:
	dune build @install

release:
	dune build @install --profile=release

install:
	dune install --profile=release

uninstall:
	dune uninstall

clean:
	dune clean

test:
	dune runtest --profile=release

# requires odoc
doc:
	dune build @doc --profile=release

check:
	dune build @check

format:
	dune build @fmt --auto-promote

reindent:
	ocp-indent --syntax cstruct -i **/*.ml*
