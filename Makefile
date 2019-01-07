
.PHONY: release build install uninstall clean test doc reindent

release:
	dune build @install --profile=release

build:
	dune build @install

install:
	install -D _build/install/default/bin/xcp-rrdd $(DESTDIR)$(SBINDIR)/xcp-rrdd

uninstall:
	rm -f $(DESTDIR)$(SBINDIR)/xcp-rrdd

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
