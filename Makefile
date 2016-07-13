.PHONY: all clean install build test reinstall uninstall distclean
all: build

NAME=xenops
J=4

ENABLE_TESTS=--enable-tests

clean:
	@rm -f setup.data setup.log setup.bin lib/version.ml
	@rm -rf _build
	@rm -f xenopsd-xc xenopsd-xenlight xenopsd-simulator xenopsd-libvirt
	@rm -f xenopsd-xc.1 xenopsd-xenlight.1 xenopsd-simulator.1 xenopsd-libvirt.1
	@rm -f *.native

-include config.mk

config.mk:
	echo Please re-run configure
	exit 1

setup.bin: setup.ml
	@ocamlopt.opt -o $@ $< || ocamlopt -o $@ $< || ocamlc -o $@ $<
	@rm -f setup.cmx setup.cmi setup.o setup.cmo

setup.data: setup.bin
	@./setup.bin -configure $(ENABLE_TESTS) $(ENABLE_XEN) $(ENABLE_XENLIGHT) $(ENABLE_LIBVIRT) $(ENABLE_XENGUESTBIN)

build: setup.data setup.bin version.ml
	@./setup.bin -build -j $(J)
ifeq ($(ENABLE_XENLIGHT),--enable-xenlight)
	ln -s ./xenops_xl_main.native xenopsd-xenlight || true
	./xenopsd-xenlight --help=groff > xenopsd-xenlight.1
endif
ifeq ($(ENABLE_LIBVIRT),--enable-libvirt)
	ln -s ./xenops_libvirt_main.native xenopsd-libvirt || true
	./xenopsd-libvirt --help=groff > xenopsd-libvirt.1
endif
	ln -s ./xenops_simulator_main.native xenopsd-simulator || true
	./xenopsd-simulator --help=groff > xenopsd-simulator.1
	ln -s ./xenops_xc_main.native xenopsd-xc || true
	./xenopsd-xc --help=groff > xenopsd-xc.1

test: build
	@./setup.bin -test

version.ml: VERSION
	echo "let version = \"$(shell cat VERSION)\"" > lib/version.ml

install:
ifeq ($(ENABLE_XENLIGHT),--enable-xenlight)
	install -D ./xenops_xl_main.native $(DESTDIR)/$(SBINDIR)/xenopsd-xenlight
	install -D ./xenopsd-xenlight.1 $(DESTDIR)/$(MANDIR)/man1/xenopsd-xenlight.1
endif
ifeq ($(ENABLE_LIBVIRT),--enable-libvirt)
	install -D ./xenops_libvirt_main.native $(DESTDIR)/$(SBINDIR)/xenopsd-libvirt
	install -D ./xenopsd-libvirt.1 $(DESTDIR)/$(MANDIR)/man1/xenopsd-libvirt.1
endif
	install -D ./xenops_simulator_main.native $(DESTDIR)/$(SBINDIR)/xenopsd-simulator
	install -D ./xenopsd-simulator.1 $(DESTDIR)/$(MANDIR)/man1/xenopsd-simulator.1
	install -D ./xenops_xc_main.native $(DESTDIR)/$(SBINDIR)/xenopsd-xc
	install -D ./xenopsd-xc.1 $(DESTDIR)/$(MANDIR)/man1/xenopsd-xc.1
	install -D ./scripts/vif $(DESTDIR)/$(LIBEXECDIR)/vif
	install -D ./scripts/vif-real $(DESTDIR)/$(LIBEXECDIR)/vif-real
	install -D ./scripts/block $(DESTDIR)/$(LIBEXECDIR)/block
	install -D ./scripts/xen-backend.rules $(DESTDIR)/$(ETCDIR)/udev/rules.d/xen-backend.rules
	install -D ./scripts/tap $(DESTDIR)/$(LIBEXECDIR)/tap
	install -D ./scripts/qemu-dm-wrapper $(DESTDIR)/$(LIBEXECDIR)/qemu-dm-wrapper
	install -D ./scripts/qemu-vif-script $(DESTDIR)/$(LIBEXECDIR)/qemu-vif-script
	install -D ./scripts/setup-vif-rules $(DESTDIR)/$(LIBEXECDIR)/setup-vif-rules
	install -D ./scripts/setup-pvs-proxy-rules $(DESTDIR)/$(LIBEXECDIR)/setup-pvs-proxy-rules
	install -D ./scripts/common.py $(DESTDIR)/$(LIBEXECDIR)/common.py
	install -D ./set_domain_uuid.native $(DESTDIR)/$(LIBEXECDIR)/set-domain-uuid
	DESTDIR=$(DESTDIR) SBINDIR=$(SBINDIR) LIBEXECDIR=$(LIBEXECDIR) ETCDIR=$(ETCDIR) ./scripts/make-custom-xenopsd.conf

reinstall: install
	@ocamlfind remove $(NAME) || true

uninstall:
	@ocamlfind remove $(NAME) || true
	rm -f $(DESTDIR)/$(SBINDIR)/xenopsd-libvirt
	rm -f $(DESTDIR)/$(SBINDIR)/xenopsd-xenlight
	rm -f $(DESTDIR)/$(SBINDIR)/xenopsd-xc
	rm -f $(DESTDIR)/$(SBINDIR)/xenopsd-simulator
	rm -f $(DESTDIR)/$(MANDIR)/man1/xenopsd-libvirt.1
	rm -f $(DESTDIR)/$(MANDIR)/man1/xenopsd-xenlight.1
	rm -f $(DESTDIR)/$(MANDIR)/man1/xenopsd-xc.1
	rm -f $(DESTDIR)/$(MANDIR)/man1/xenopsd-simluator.1
	rm -f $(DESTDIR)/$(ETCDIR)/xenopsd.conf
	rm -f $(DESTDIR)/$(LIBEXECDIR)/vif
	rm -f $(DESTDIR)/$(LIBEXECDIR)/vif-real
	rm -f $(DESTDIR)/$(LIBEXECDIR)/block
	rm -f $(DESTDIR)/$(ETCDIR)/udev/rules.d/xen-backend.rules
	rm -f $(DESTDIR)/$(LIBEXECDIR)/tap
	rm -f $(DESTDIR)/$(LIBEXECDIR)/qemu-dm-wrapper
	rm -f $(DESTDIR)/$(LIBEXECDIR)/qemu-vif-script
	rm -f $(DESTDIR)/$(LIBEXECDIR)/setup-vif-rules
	rm -f $(DESTDIR)/$(LIBEXECDIR)/setup-pvs-proxy-rules
	rm -f $(DESTDIR)/$(LIBEXECDIR)/common.py*

.PHONY: release
release:
	# remove -warn-error
	grep -v 'warn-error' _oasis > _oasis.tmp
	mv _oasis.tmp _oasis
	oasis setup

# make coverage - prepares for building with coverage analysis
# make uncover  - reverses the setup from "make coverage"
# make report   - create coverage/index.html 

coverage: _tags _tags.coverage 
	test ! -f _tags.orig && mv _tags _tags.orig || true
	cat _tags.coverage _tags.orig > _tags

uncover: _tags.orig
	mv _tags.orig _tags

report:
	bisect-ppx-report -I _build -html coverage /tmp/bisect-xenops*out

.PHONY: report coverage uncover
	
