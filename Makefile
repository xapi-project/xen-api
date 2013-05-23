.PHONY: all clean install build reinstall uninstall distclean
all: build

NAME=xenops
J=4

clean:
	@obuild clean
	@rm -f setup.data setup.log setup.bin

distclean: clean
	@rm -f config.mk

-include config.mk

export OCAMLRUNPARAM=b

TESTS     := --enable-tests
TESTS     := --disable-tests
XEN       ?= $(shell if ocamlfind query xenctrl >/dev/null 2>&1; then echo --enable-xen; fi)
XEN := --enable-xen
SIMULATOR := --enable-simulator

.PHONY: build
build: dist/setup
	obuild build

dist/setup: xenopsd.obuild config.mk
	obuild configure

config.mk:
	@echo
	@echo "Please run configure before building"
	@echo
	@exit 1

install:
	install -D ./dist/build/xenopsd_libvirt/xenopsd_libvirt $(DESTDIR)/$(SBINDIR)/xenopsd_libvirt
	install -D ./dist/build/xenopsd_qemu/xenopsd_qemu $(DESTDIR)/$(SBINDIR)/xenopsd_qemu
	install -D ./dist/build/xenopsd/xenopsd $(DESTDIR)/$(SBINDIR)/xenopsd
	install -D ./dist/build/xenopsd_simulator/xenopsd_simulator $(DESTDIR)/$(SBINDIR)/xenopsd_simulator
	install -D ./dist/build/xenguest/xenguest $(DESTDIR)/$(LIBEXECDIR)/xenguest
	install -D ./scripts/vif $(DESTDIR)/$(SCRIPTSDIR)/vif
	install -D ./scripts/qemu-dm-wrapper $(DESTDIR)/$(LIBEXECDIR)/qemu-dm-wrapper
	install -D ./scripts/qemu-vif-script $(DESTDIR)/$(LIBEXECDIR)/qemu-vif-script
	install -D ./scripts/setup-vif-rules $(DESTDIR)/$(LIBEXECDIR)/setup-vif-rules
	install -D ./scripts/common.py $(DESTDIR)/$(LIBEXECDIR)/common.py
	install -D ./scripts/network.conf $(DESTDIR)/$(ETCDIR)/xcp/network.conf
	DESTDIR=$(DESTDIR) SBINDIR=$(SBINDIR) LIBEXECDIR=$(LIBEXECDIR) SCRIPTSDIR=$(SCRIPTSDIR) ETCDIR=$(ETCDIR) ./scripts/make-custom-xenopsd.conf

reinstall: install
	@ocamlfind remove $(NAME) || true

uninstall:
	@ocamlfind remove $(NAME) || true
	rm -f $(DESTDIR)/$(SBINDIR)/xenopsd_libvirt
	rm -f $(DESTDIR)/$(SBINDIR)/xenopsd_qemu
	rm -f $(DESTDIR)/$(SBINDIR)/xenopsd
	rm -f $(DESTDIR)/$(SBINDIR)/xenopsd_simulator
	rm -f $(DESTDIR)/$(LIBEXECDIR)/xenguest
	rm -f $(DESTDIR)/$(ETCDIR)/xenopsd.conf
	rm -f $(DESTDIR)/$(SCRIPTSDIR)/vif
	rm -f $(DESTDIR)/$(LIBEXECDIR)/qemu-dm-wrapper
	rm -f $(DESTDIR)/$(LIBEXECDIR)/setup-vif-rules
	rm -f $(DESTDIR)/$(ETCDIR)/xcp/network.conf

