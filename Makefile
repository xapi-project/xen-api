OPAM_PREFIX?=$(DESTDIR)$(shell opam config var prefix)
OPAM_LIBDIR?=$(DESTDIR)$(shell opam config var lib)

.PHONY: release build install uninstall clean test doc reindent

release:
	dune build -p xapi-tapctl -j $$(getconf _NPROCESSORS_ONLN)

build:
	dune build @install

install:
	dune install --prefix=$(OPAM_PREFIX) --libdir=$(OPAM_LIBDIR) -p xapi-tapctl

uninstall:
	dune uninstall --prefix=$(OPAM_PREFIX) --libdir=$(OPAM_LIBDIR) -p xapi-tapctl

clean:
	dune clean

test:
	dune runtest

# requires odoc
doc:
	dune build @doc -profile=release

reindent:
	git ls-files '*.ml*' | xargs ocp-indent --syntax cstruct -i


.DEFAULT_GOAL := release
