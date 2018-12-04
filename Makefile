BINDIR ?= /usr/bin
SBINDIR ?= /usr/sbin
MANDIR ?= /usr/share/man/man1
JOBS = $(shell getconf _NPROCESSORS_ONLN)
PROFILE=release


.PHONY: release build install uninstall clean test doc reindent

release:
	dune build @install @networkd/man --profile=$(PROFILE) -j $(JOBS)

build:
	dune build @install @networkd/man -j $(JOBS)

install:
	mkdir -p $(DESTDIR)$(SBINDIR)
	cp _build/default/networkd/networkd.exe $(DESTDIR)$(SBINDIR)/xcp-networkd
	mkdir -p $(DESTDIR)$(MANDIR)
	cp _build/default/networkd/xcp-networkd.1 $(DESTDIR)$(MANDIR)/xcp-networkd.1
	mkdir -p $(DESTDIR)$(BINDIR)
	cp _build/default/networkd_db/networkd_db.exe $(DESTDIR)$(BINDIR)/networkd_db

uninstall:
	rm -f $(DESTDIR)$(SBINDIR)/xcp-networkd
	rm -f $(DESTDIR)$(MANDIR)/xcp-networkd.1
	rm -f $(DESTDIR)$(SBINDIR)/networkd_db

clean:
	dune clean

test:
	dune runtest --profile=$(PROFIE)

# requires odoc
doc:
	dune build @doc --profile=$(PROFILE)

reindent:
	ocp-indent --inplace **/*.ml*
