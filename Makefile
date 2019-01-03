
PROFILE=release
.PHONY: build install uninstall clean reindent

build:
	dune build @install --profile=$(PROFILE)

install:
	dune install

uninstall:
	dune uninstall

clean:
	dune clean

reindent:
	ocp-indent --inplace **/*.ml
