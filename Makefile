
PROFILE=release

.PHONY: build install uninstall test clean reindent

build:
	dune build @install --profile=$(PROFILE)

install:
	dune install

uninstall:
	dune uninstall

test:
	dune runtest --profile=$(PROFILE)

clean:
	dune clean

reindent:
	git ls-files '**/*.ml' | xargs ocp-indent --inplace

