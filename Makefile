OPAM_PREFIX=$(DESTDIR)$(shell opam config var prefix)
OPAM_LIBDIR=$(DESTDIR)$(shell opam config var lib)

.PHONY: build release install uninstall clean test doc reindent

release:
	    dune build @install -j $$(getconf _NPROCESSORS_ONLN)

build:
	    dune build @install --dev -j $$(getconf _NPROCESSORS_ONLN)


install:
	    dune install --prefix=$(OPAM_PREFIX) --libdir=$(OPAM_LIBDIR)

uninstall:
	    dune uninstall

clean:
	    dune clean

test:
	    dune runtest --dev -j $$(getconf _NPROCESSORS_ONLN)

# requires odoc
doc:
	    dune build @doc

gh-pages:
	    bash .docgen.sh

reindent:
	    git ls-files '*.ml' '*.mli' | xargs ocp-indent --syntax cstruct -i

runtime-coverage:
	    BISECT_RUNTIME=YES make

.DEFAULT_GOAL := release
