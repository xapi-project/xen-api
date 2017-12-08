DESTDIR ?= 
BINDIR ?= /opt/xensource/bin
MANDIR ?= /opt/xensource/man/man1

all: build


build:
	jbuilder build

install: build
	mkdir -p $(DESTDIR)$(BINDIR)
	install -m 755 _build/install/default/bin/rrd2csv $(DESTDIR)$(BINDIR)/rrd2csv
	mkdir -p $(DESTDIR)$(MANDIR)
	install -m 644 man/rrd2csv.1.man $(DESTDIR)$(MANDIR)/rrd2csv.1.man

uninstall:
	rm -f $(DESTDIR)$(BINDIR)/rrd2csv
	rm -f $(DESTDIR)$(MANDIR)/rrd2csv.1.man

clean:
	jbuilder clean

.PHONY: all build install uninstall clean
