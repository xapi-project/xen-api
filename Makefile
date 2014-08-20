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
	@rm -f configure.cm*

setup.bin: setup.ml
	@ocamlopt.opt -o $@ $< || ocamlopt -o $@ $< || ocamlc -o $@ $<
	@rm -f setup.cmx setup.cmi setup.o setup.cmo

setup.data: setup.bin
	@./setup.bin -configure $(ENABLE_XEN) $(ENABLE_XENLIGHT) $(ENABLE_LIBVIRT) $(ENABLE_XENGUESTBIN)

build: setup.data setup.bin
	@./setup.bin -build -j $(J)

install:
ifeq ($(ENABLE_XENLIGHT),--enable-xenlight)
	install -D ./xenops_xl_main.native $(DESTDIR)/$(SBINDIR)/xenopsd-xl
endif
ifeq ($(ENABLE_LIBVIRT),--enable-libvirt)
	install -D ./xenops_libvirt_main.native $(DESTDIR)/$(SBINDIR)/xenopsd-libvirt
endif
	install -D ./xenops_simulator_main.native $(DESTDIR)/$(SBINDIR)/xenopsd-simulator
	install -D ./xenops_xc_main.native $(DESTDIR)/$(SBINDIR)/xenopsd-xc
	install -D ./scripts/vif $(DESTDIR)/$(SCRIPTSDIR)/vif
	install -D ./scripts/block $(DESTDIR)/$(SCRIPTSDIR)/block
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
	rm -f $(DESTDIR)/$(SBINDIR)/xenopsd-libvirt
	rm -f $(DESTDIR)/$(SBINDIR)/xenopsd-xenlight
	rm -f $(DESTDIR)/$(SBINDIR)/xenopsd-xc
	rm -f $(DESTDIR)/$(SBINDIR)/xenopsd-simulator
	rm -f $(DESTDIR)/$(ETCDIR)/xenopsd.conf
	rm -f $(DESTDIR)/$(SCRIPTSDIR)/vif
	rm -f $(DESTDIR)/$(SCRIPTSDIR)/block
	rm -f $(DESTDIR)/$(LIBEXECDIR)/qemu-dm-wrapper
	rm -f $(DESTDIR)/$(LIBEXECDIR)/setup-vif-rules
	rm -f $(DESTDIR)/$(ETCDIR)/xcp/network.conf

