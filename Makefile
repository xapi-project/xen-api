OPAM_PREFIX=$(DESTDIR)$(shell opam config var prefix)
OPAM_LIBDIR=$(DESTDIR)$(shell opam config var lib)

.PHONY: build release install uninstall clean test doc reindent

release:
	dune build @install -j $$(getconf _NPROCESSORS_ONLN) --profile=release

build:
	dune build @install -j $$(getconf _NPROCESSORS_ONLN)


install:
	dune install --prefix=$(OPAM_PREFIX) --libdir=$(OPAM_LIBDIR) --profile=release

uninstall:
	dune uninstall --profile=release

clean:
	dune clean

test:
	dune runtest -j $$(getconf _NPROCESSORS_ONLN) --profile=release

# requires odoc
doc:
	dune build @doc --profile=release

gh-pages:
	bash .docgen.sh

reindent:
	git ls-files '*.ml' '*.mli' | xargs ocp-indent --syntax cstruct -i

runtime-coverage:
	BISECT_RUNTIME=YES make

.DEFAULT_GOAL := release
