OPAM_PREFIX?=$(DESTDIR)$(shell opam config var prefix)
OPAM_LIBDIR?=$(DESTDIR)$(shell opam config var lib)

.PHONY: release build install uninstall clean test doc reindent

release:
	dune build @install --profile=release

build:
	dune build @install

install:
	dune install --prefix=$(OPAM_PREFIX) --libdir=$(OPAM_LIBDIR) forkexec --profile=release
	install -D _build/install/default/bin/forkexecd $(DESTDIR)$(SBINDIR)/forkexecd
	install -D _build/install/default/bin/forkexecd-cli $(DESTDIR)$(SBINDIR)/forkexecd-cli

uninstall:
	dune uninstall --prefix=$(OPAM_PREFIX) --libdir=$(OPAM_LIBDIR) --profile=release
	rm -f $(DESTDIR)$(SBINDIR)/forkexecd
	rm -f $(DESTDIR)$(SBINDIR)/forkexecd-cli

clean:
	dune clean

test:
	dune runtest --profile=release

# requires odoc
doc:
	dune build @doc --profile=release

reindent:
	git ls-files '*.ml*' | xargs ocp-indent --inplace
