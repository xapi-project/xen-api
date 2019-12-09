
.PHONY: release build install uninstall clean reindent
PROFILE=release

build:
	dune build @install --profile=$(PROFILE)

release:
	dune build @install --profile=$(PROFILE)

install:
	dune install xenops
	dune install xapi-xenops
	install -D _build/install/default/bin/list_domains $(DESTDIR)$(BINDIR)/list_domains

uninstall:
	dune uninstall
	rm -f $(DESTDIR)$(BINDIR)/list_domains

clean:
	dune clean

reindent:
	git ls-files '**/*.ml' | xargs ocp-indent --inplace

