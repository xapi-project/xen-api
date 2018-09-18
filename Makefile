
.PHONY: build release install uninstall clean reindent

build:
	dune build @install

release:
	dune build @install --profile release

install:
	dune install

uninstall:
	dune uninstall

clean:
	dune clean

test:
	dune runtest

reindent:
	ocp-indent --inplace **/*.ml*
