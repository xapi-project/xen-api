PROFILE=release
.PHONY: build install uninstall clean test doc reindent

build:
	dune build @install --profile=$(PROFILE)

install:
	dune install

uninstall:
	dune uninstall

clean:
	dune clean

test:
	dune runtest --profile=$(PROFILE)

reindent:
	ocp-indent --syntax cstruct -i **/*.mli
	ocp-indent --syntax cstruct -i **/*.ml
