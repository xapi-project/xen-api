BINDIR ?= /usr/bin
SBINDIR ?= /usr/sbin
ETCDIR ?= /etc

.PHONY: install uninstall clean

dist/build/xcp-rrdd/xcp-rrdd:
	obuild configure
	obuild build

install:
	install -D dist/build/xcp-rrdd/xcp-rrdd $(DESTDIR)$(SBINDIR)/xcp-rrdd

uninstall:
	rm -f $(DESTDIR)$(SBINDIR)/xcp-rrdd

clean:
	rm -rf dist
