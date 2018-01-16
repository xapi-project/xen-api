DESTDIR ?=
ETCDIR ?= /etc/xensource
LIBEXECDIR ?= /opt/xensource/libexec

.PHONY: release build install uninstall clean doc reindent

release:
	jbuilder build @install

build:
	jbuilder build @install --dev

install:
	install -D -m 755 _build/install/default/bin/xcp-rrdd-iostat $(DESTDIR)$(LIBEXECDIR)/xcp-rrdd-plugins/xcp-rrdd-iostat
	install -D -m 755 _build/install/default/bin/xcp-rrdd-squeezed $(DESTDIR)$(LIBEXECDIR)/xcp-rrdd-plugins/xcp-rrdd-squeezed
	install -D -m 755 _build/install/default/bin/xcp-rrdd-xenpm $(DESTDIR)$(LIBEXECDIR)/xcp-rrdd-plugins/xcp-rrdd-xenpm
	install -D -m 644 bugtool-plugin/rrdd-plugins.xml $(DESTDIR)$(ETCDIR)/bugtool/xcp-rrdd-plugins.xml
	install -D -m 644 bugtool-plugin/rrdd-plugins/stuff.xml $(DESTDIR)$(ETCDIR)/bugtool/xcp-rrdd-plugins/stuff.xml
	install -D -m 755 scripts/sysconfig-rrdd-plugins $(DESTDIR)/etc/sysconfig/xcp-rrdd-plugins
	install -D -m 644 scripts/logrotate-rrdd-plugins $(DESTDIR)/etc/logrotate.d/xcp-rrdd-plugins

uninstall:
	rm -f $(DESTDIR)$(LIBEXECDIR)/xcp-rrdd-plugins/xcp-rrdd-iostat
	rm -f $(DESTDIR)$(LIBEXECDIR)/xcp-rrdd-plugins/xcp-rrdd-squeezed
	rm -f $(DESTDIR)$(LIBEXECDIR)/xcp-rrdd-plugins/xcp-rrdd-xenpm
	rm -f $(DESTDIR)$(ETCDIR)/bugtool/xcp-rrdd-plugins.xml
	rm -f $(DESTDIR)$(ETCDIR)/bugtool/xcp-rrdd-plugins/stuff.xml
	rm -f $(DESTDIR)/etc/sysconfig/xcp-rrdd-plugins
	rm -f $(DESTDIR)/etc/logrotate.d/xcp-rrdd-plugins

clean:
	jbuilder clean

# requires odoc
doc:
	jbuilder build @doc

reindent:
	git ls-files '*.ml*' | xargs ocp-indent --syntax cstruct -i


.DEFAULT_GOAL := release
