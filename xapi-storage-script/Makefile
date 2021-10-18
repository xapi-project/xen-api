BINDIR=/usr/sbin
MANDIR=/usr/share/man
DESTDIR?=

.PHONY: release build install uninstall clean doc reindent test

release:
	dune build @install --profile=release

build:
	dune build @install

install:
	install -m 0755 _build/install/default/bin/xapi-storage-script $(DESTDIR)$(BINDIR)
	install -m 0644 _build/install/default/man/man8/xapi-storage-script.8 $(DESTDIR)$(MANDIR)/man8

uninstall:
	rm -f $(DESTDIR)$(BINDIR)/xapi-storage-script
	rm -f $(DESTDIR)$(MANDIR)/man8/xapi-storage-script.8*

clean:
	dune clean

# requires odoc
doc:
	dune build @doc --profile=release

format:
	dune build @fmt --auto-promote

test:
	dune runtest --profile=release

.DEFAULT_GOAL := release
