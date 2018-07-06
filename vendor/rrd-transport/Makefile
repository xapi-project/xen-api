OPAM_PREFIX?=$(DESTDIR)$(shell opam config var prefix)
OPAM_LIBDIR?=$(DESTDIR)$(shell opam config var lib)

.PHONY: release build install uninstall clean test doc reindent

release:
	jbuilder build @install

build:
	jbuilder build @install --dev

install:
	jbuilder install --prefix=$(OPAM_PREFIX) --libdir=$(OPAM_LIBDIR) rrd-transport
	install -D _build/install/default/bin/rrdreader $(DESTDIR)$(BINDIR)/rrdreader
	install -D _build/install/default/bin/rrdwriter $(DESTDIR)$(BINDIR)/rrdwriter

uninstall:
	jbuilder uninstall --prefix=$(OPAM_PREFIX) --libdir=$(OPAM_LIBDIR)
	rm -f $(DESTDIR)$(BINDIR)/rrdreader
	rm -f $(DESTDIR)$(BINDIR)/rrdwriter

clean:
	jbuilder clean

test:
	jbuilder runtest

# requires odoc
doc:
	jbuilder build @doc

gh-pages:
	bash .docgen.sh

reindent:
	git ls-files '*.ml*' | xargs ocp-indent --inplace
