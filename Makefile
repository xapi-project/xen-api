.PHONY: all clean install build reinstall uninstall distclean
all: build

NAME=xenops
J=4

clean:
	@rm -f setup.data setup.log setup.bin config.mk
	@rm -rf _build

-include config.mk

config.mk: configure
	./configure

configure: configure.ml
	ocamlfind ocamlc -linkpkg -package findlib,cmdliner -o configure configure.ml

setup.bin: setup.ml
	@ocamlopt.opt -o $@ $< || ocamlopt -o $@ $< || ocamlc -o $@ $<
	@rm -f setup.cmx setup.cmi setup.o setup.cmo

setup.data: setup.bin
	@./setup.bin -configure $(ENABLE_XEN) $(ENABLE_XENLIGHT) $(ENABLE_LIBVIRT)

build: setup.data setup.bin
	@./setup.bin -build -j $(J)

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

