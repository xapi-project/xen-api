OPAM_PREFIX?=$(DESTDIR)$(shell opam config var prefix)
OPAM_LIBDIR?=$(DESTDIR)$(shell opam config var lib)

.PHONY: release build install uninstall clean test doc reindent

release:
	dune build @install --profile=release

build:
	dune build @install --dev

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

reindent:
	git ls-files '*.ml' '*.mli' | xargs ocp-indent --inplace
