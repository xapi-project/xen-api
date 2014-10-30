DESTDIR ?= 
BINDIR ?= /opt/xensource/bin
MANDIR ?= /opt/xensource/man/man1

SETUP = ocaml setup.ml

all: build

setup.data:
	$(SETUP) -configure $(CONFIGUREFLAGS)

build: setup.data
	$(SETUP) -build $(BUILDFLAGS)

test: build
	$(SETUP) -test $(TESTFLAGS)

install: build
	mkdir -p $(DESTDIR)$(BINDIR)
	install -m 755 rrd2csv.native $(DESTDIR)$(BINDIR)/rrd2csv
	mkdir -p $(DESTDIR)$(MANDIR)
	install -m 644 man/rrd2csv.1.man $(DESTDIR)$(MANDIR)/rrd2csv.1.man

uninstall: setup.data
	rm -f $(DESTDIR)$(BINDIR)/rrd2csv
	rm -f $(DESTDIR)$(MANDIR)/rrd2csv.1.man

clean:
	$(SETUP) -clean $(CLEANFLAGS)

.PHONY: all build test install uninstall clean
