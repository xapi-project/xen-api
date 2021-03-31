.PHONY: release build install uninstall clean test doc format

release:
	dune build @install --profile=release

build:
	dune build @install

install:
	# rrdd
	install -D _build/install/default/bin/xcp-rrdd $(DESTDIR)$(SBINDIR)/xcp-rrdd
	# transport
	dune install rrd-transport
	install -D _build/install/default/bin/rrdreader $(DESTDIR)$(BINDIR)/rrdreader
	install -D _build/install/default/bin/rrdwriter $(DESTDIR)$(BINDIR)/rrdwriter
	# rrdd-plugin
	dune install rrdd-plugin

uninstall:
	# rrdd
	rm -f $(DESTDIR)$(SBINDIR)/xcp-rrdd
	# transport
	dune uninstall --libdir=$(OPAM_LIBDIR)
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

format:
	dune build @fmt --auto-promote

verify-cert:
	@NONE=$$( git grep -r --count 'verify_cert:None' -- **/*.ml | cut -d ':' -f 2 | paste -sd+ - | bc) ;\
	echo "counted $$NONE usages of verify_cert:None" ;\
	test $$NONE -eq 1
