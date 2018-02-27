BINDIR=/usr/sbin
MANDIR=/usr/share/man

.PHONY: release build install uninstall clean doc reindent test

release:
	jbuilder build @install

build:
	jbuilder build @install --dev

install:
	install -m 0755 _build/install/default/bin/xapi-storage-script $(BINDIR)
	install -m 0644 _build/install/default/man/man8/xapi-storage-script.8 $(MANDIR)/man8

uninstall:
	rm -f $(BINDIR)/xapi-storage-script
	rm -f $(MANDIR)/man8/xapi-storage-script.8*

clean:
	jbuilder clean

# requires odoc
doc:
	jbuilder build @doc

reindent:
	git ls-files '*.ml*' | xargs ocp-indent --syntax cstruct -i

test:
	jbuilder runtest

.DEFAULT_GOAL := release
