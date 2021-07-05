
URL ?= https://github.com/xapi-project/xen-api
BRANCH ?= master

.PHONY: release build async-examples lwt-examples install uninstall clean test doc reindent regenerate

release:
	dune build @install --profile=release

build:
	dune build @install

async-examples:
	dune build @async_examples/examples

lwt-examples:
	dune build @lwt_examples/examples

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

gh-pages:
	bash .docgen.sh

reindent:
	git ls-files '*.ml' '*.mli' | xargs ocp-indent --syntax cstruct -i

