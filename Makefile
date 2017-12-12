OPAM_PREFIX?=$(DESTDIR)$(shell opam config var prefix)
OPAM_LIBDIR?=$(DESTDIR)$(shell opam config var lib)

.PHONY: release build install uninstall clean test doc reindent

release:
	jbuilder build @install

build:
	jbuilder build @install --dev

install:
	install -D _build/install/default/bin/xcp-rrdd $(DESTDIR)$(SBINDIR)/xcp-rrdd

uninstall:
	rm -f $(DESTDIR)$(SBINDIR)/xcp-rrdd

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
