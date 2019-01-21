.PHONY: release build install uninstall clean test doc reindent

release:
	dune build @install --profile=release

build:
	dune build @install

install:
	dune install forkexec --profile=release
	install -D _build/install/default/bin/forkexecd $(DESTDIR)$(SBINDIR)/forkexecd
	install -D _build/install/default/bin/forkexecd-cli $(DESTDIR)$(SBINDIR)/forkexecd-cli

uninstall:
	dune uninstall --profile=release
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
