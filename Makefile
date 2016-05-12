DESTDIR ?=
ETCDIR ?= /etc/xensource
LIBEXECDIR ?= /opt/xensource/libexec

SETUP = ocaml setup.ml

build: setup.data
	$(SETUP) -build $(BUILDFLAGS)

doc: setup.data build
	$(SETUP) -doc $(DOCFLAGS)

test: setup.data build
	$(SETUP) -test $(TESTFLAGS)

clean:
	$(SETUP) -clean $(CLEANFLAGS)

distclean:
	$(SETUP) -distclean $(DISTCLEANFLAGS)

setup.data:
	$(SETUP) -configure $(CONFIGUREFLAGS)

configure:
	$(SETUP) -configure $(CONFIGUREFLAGS)

.PHONY: build doc test clean distclean configure install

install: build
	install -D -m 755 rrdp_iostat.native $(DESTDIR)$(LIBEXECDIR)/xcp-rrdd-plugins/xcp-rrdd-iostat
	install -D -m 755 rrdp_squeezed.native $(DESTDIR)$(LIBEXECDIR)/xcp-rrdd-plugins/xcp-rrdd-squeezed
	install -D -m 755 rrdp_xenpm.native $(DESTDIR)$(LIBEXECDIR)/xcp-rrdd-plugins/xcp-rrdd-xenpm
	install -D -m 644 bugtool-plugin/rrdd-plugins.xml $(DESTDIR)$(ETCDIR)/bugtool/xcp-rrdd-plugins.xml
	install -D -m 644 bugtool-plugin/rrdd-plugins/stuff.xml $(DESTDIR)$(ETCDIR)/bugtool/xcp-rrdd-plugins/stuff.xml
	install -D -m 755 scripts/init.d-rrdd-plugins $(DESTDIR)/etc/rc.d/init.d/xcp-rrdd-plugins
	install -D -m 755 scripts/sysconfig-rrdd-plugins $(DESTDIR)/etc/sysconfig/xcp-rrdd-plugins
	install -D -m 644 scripts/logrotate-rrdd-plugins $(DESTDIR)/etc/logrotate.d/xcp-rrdd-plugins
