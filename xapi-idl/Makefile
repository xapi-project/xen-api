
.PHONY: build release install uninstall clean test doc reindent

PROFILE=release

release:
	dune build @install  --profile=$(PROFILE)

build:
	dune build @install

install:
	dune install --profile=$(PROFILE)

uninstall:
	dune uninstall --profile=$(PROFILE)

clean:
	dune clean

test:
	dune runtest  --profile=$(PROFILE)

test-quick:
	dune build @runtest-quick  --profile=$(PROFILE)

all:
	dune build @all --profile=$(PROFILE)

# requires odoc
doc:
	dune build @doc --profile=$(PROFILE)

gh-pages:
	bash .docgen.sh

# requires ocamlformat
format:
	dune build @fmt --auto-promote

reindent:
	git ls-files '*.ml' '*.mli' | xargs ocp-indent --syntax cstruct -i

runtime-coverage:
	BISECT_RUNTIME=YES make

.DEFAULT_GOAL := release
