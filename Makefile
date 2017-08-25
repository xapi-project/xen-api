OPAM_PREFIX?=$(DESTDIR)$(shell opam config var prefix)
OPAM_LIBDIR?=$(DESTDIR)$(shell opam config var lib)

.PHONY: release build install uninstall clean reindent

build:
	jbuilder build @install --dev

release:
	jbuilder build @install

install:
	jbuilder install --prefix=$(OPAM_PREFIX) --libdir=$(OPAM_LIBDIR) xenops
	jbuilder install --prefix=$(OPAM_PREFIX) --libdir=$(OPAM_LIBDIR) xapi-xenops
	install -D _build/install/default/bin/list_domains $(DESTDIR)$(BINDIR)/list_domains

uninstall:
	jbuilder uninstall --prefix=$(OPAM_PREFIX) --libdir=$(OPAM_LIBDIR)
	rm -f $(DESTDIR)$(BINDIR)/list_domains

clean:
	jbuilder clean

reindent:
	ocp-indent --inplace **/*.ml

.DEFAULT_GOAL := release
