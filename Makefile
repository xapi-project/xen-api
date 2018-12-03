OPAM_PREFIX?=$(DESTDIR)$(shell opam config var prefix)
OPAM_LIBDIR?=$(DESTDIR)$(shell opam config var lib)

.PHONY: release build install uninstall clean reindent

build:
	dune build @install

release:
	dune build @install --profile=release

install:
	dune install --prefix=$(OPAM_PREFIX) --libdir=$(OPAM_LIBDIR) xenops
	dune install --prefix=$(OPAM_PREFIX) --libdir=$(OPAM_LIBDIR) xapi-xenops
	install -D _build/install/default/bin/list_domains $(DESTDIR)$(BINDIR)/list_domains

uninstall:
	dune uninstall --prefix=$(OPAM_PREFIX) --libdir=$(OPAM_LIBDIR)
	rm -f $(DESTDIR)$(BINDIR)/list_domains

clean:
	dune clean

reindent:
	git ls-files '**/*.ml' | xargs ocp-indent --inplace

