OPAM_PREFIX?=$(DESTDIR)$(shell opam config var prefix)
OPAM_LIBDIR?=$(DESTDIR)$(shell opam config var lib)

.PHONY: release build install uninstall clean test doc reindent

release:
	dune build @install --profile=release

build:
	dune build @install

install:
	dune install --prefix=$(OPAM_PREFIX) --libdir=$(OPAM_LIBDIR) rrd-transport
	install -D _build/install/default/bin/rrdreader $(DESTDIR)$(BINDIR)/rrdreader
	install -D _build/install/default/bin/rrdwriter $(DESTDIR)$(BINDIR)/rrdwriter

uninstall:
	dune uninstall --prefix=$(OPAM_PREFIX) --libdir=$(OPAM_LIBDIR)
	rm -f $(DESTDIR)$(BINDIR)/rrdreader
	rm -f $(DESTDIR)$(BINDIR)/rrdwriter

clean:
	dune clean

test:
	dune runtest --profile=release

# requires odoc
doc:
	dune build @doc --profile=release

gh-pages:
	bash .docgen.sh

reindent:
	git ls-files '*.ml*' | xargs ocp-indent --inplace
