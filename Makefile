BINDIR ?= /usr/bin
SBINDIR ?= /usr/sbin
MANDIR ?= /usr/share/man/man1

.PHONY: build release install uninstall clean test doc reindent

build:
	jbuilder build @networkd/man @install --dev

release:
	jbuilder build @install

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
	jbuilder clean

test:
	_build/default/test/network_test.exe

# requires odoc
doc:
	jbuilder build @doc

reindent:
	ocp-indent --inplace **/*.ml*
