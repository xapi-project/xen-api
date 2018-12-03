DESTDIR ?=
BINDIR ?= /opt/xensource/bin
MANDIR ?= /opt/xensource/man/man1

all: build

build:
	dune build @install

release:
	dune build @install --profile=release

install: build
	mkdir -p $(DESTDIR)$(BINDIR)
	install -m 755 _build/install/default/bin/rrd2csv $(DESTDIR)$(BINDIR)/rrd2csv
	mkdir -p $(DESTDIR)$(MANDIR)
	install -m 644 man/rrd2csv.1.man $(DESTDIR)$(MANDIR)/rrd2csv.1.man

uninstall:
	rm -f $(DESTDIR)$(BINDIR)/rrd2csv
	rm -f $(DESTDIR)$(MANDIR)/rrd2csv.1.man

clean:
	dune clean

.PHONY: all build release install uninstall clean
.DEFAULT_GOAL := release
