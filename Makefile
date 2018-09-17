OPAM_PREFIX?=$(DESTDIR)$(shell opam config var prefix)
OPAM_LIBDIR?=$(DESTDIR)$(shell opam config var lib)

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
	dune install --prefix=$(OPAM_PREFIX) --libdir=$(OPAM_LIBDIR)

uninstall:
	dune uninstall --prefix=$(OPAM_PREFIX) --libdir=$(OPAM_LIBDIR)

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

