.PHONY: all clean install build test libinstall reinstall uninstall distclean
all: build

NAME=xenopsd
J=4

ENABLE_TESTS=--enable-tests
COVERAGE=coverage

clean:
	@rm -f setup.data setup.log setup.bin setup.ml _oasis lib/version.ml
	@rm -rf _build
	@rm -f xenopsd-xc xenopsd-xenlight xenopsd-simulator xenopsd-libvirt
	@rm -f xenopsd-xc.1 xenopsd-xenlight.1 xenopsd-simulator.1 xenopsd-libvirt.1
	@rm -f *.native
	@rm -f /tmp/bisect-xenops*.out || true
	@rm -rf $(COVERAGE)

-include config.mk

config.mk:
	echo Please re-run configure
	exit 1

setup.bin: setup.ml
	@ocamlfind ocamlopt -o $@ -linkpkg -package oasis.dynrun setup.ml || ocamlfind ocamlc -o $@ -linkpkg -package oasis.dynrun setup.ml
	@rm -f setup.cmi setup.cmo setup.cmx setup.o

setup.data: setup.bin
	@./setup.bin -configure $(ENABLE_TESTS) $(ENABLE_XEN) $(ENABLE_XENLIGHT) $(ENABLE_XENGUESTBIN) $(ENABLE_XENTOOLLOG)

setup.ml: _oasis.in
ifeq ($(BISECT_COVERAGE),YES)
	rm -f _oasis
	sed -e 's/BuildDepends:/BuildDepends: bisect_ppx,/' _oasis.in >_oasis
else
	ln -sf _oasis.in _oasis
endif
	oasis setup -setup-update dynamic

_build/config.ml: config.ml
	@mkdir -p _build
	@cp config.ml _build/

build: setup.data setup.bin version.ml _build/config.ml
	@./setup.bin -build -j $(J)
ifeq ($(ENABLE_XENLIGHT),--enable-xenlight)
	ln -s ./xenops_xl_main.native xenopsd-xenlight || true
	./xenopsd-xenlight --help=groff > xenopsd-xenlight.1
endif
	ln -s ./xenops_simulator_main.native xenopsd-simulator || true
	./xenopsd-simulator --help=groff > xenopsd-simulator.1
	ln -s ./xenops_xc_main.native xenopsd-xc || true
	./xenopsd-xc --help=groff > xenopsd-xc.1

test: build
	@./setup.bin -test

version.ml: VERSION
	echo "let version = \"$(shell cat VERSION)\"" > lib/version.ml

libinstall: build
	@./setup.bin -install

install: libinstall
ifeq ($(ENABLE_XENLIGHT),--enable-xenlight)
	install -D ./xenops_xl_main.native $(DESTDIR)/$(SBINDIR)/xenopsd-xenlight
	install -D ./xenopsd-xenlight.1 $(DESTDIR)/$(MANDIR)/man1/xenopsd-xenlight.1
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
	install -D ./scripts/igmp_query_injector.py $(DESTDIR)/$(LIBEXECDIR)/igmp_query_injector.py
	install -D ./set_domain_uuid.native $(DESTDIR)/$(LIBEXECDIR)/set-domain-uuid
	install -D ./scripts/qemu-wrapper $(DESTDIR)/$(QEMU_WRAPPER_DIR)/qemu-wrapper
	DESTDIR=$(DESTDIR) SBINDIR=$(SBINDIR) QEMU_WRAPPER_DIR=$(QEMU_WRAPPER_DIR) LIBEXECDIR=$(LIBEXECDIR) ETCDIR=$(ETCDIR) ./scripts/make-custom-xenopsd.conf

reinstall: install
	@ocamlfind remove $(NAME) || true

uninstall:
	@ocamlfind remove $(NAME) || true
	rm -f $(DESTDIR)/$(SBINDIR)/xenopsd-xenlight
	rm -f $(DESTDIR)/$(SBINDIR)/xenopsd-xc
	rm -f $(DESTDIR)/$(SBINDIR)/xenopsd-simulator
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
	rm -f $(DESTDIR)/$(LIBEXECDIR)/igmp_query_injector.py*
	rm -f $(DESTDIR)/$(QEMU_WRAPPER_DIR)/qemu-wrapper

.PHONY: release
release:
	# remove -warn-error
	grep -v 'warn-error' _oasis > _oasis.tmp
	mv _oasis.tmp _oasis
	oasis setup

# make report   - create coverage/index.html

report:
	bisect-ppx-report -I _build -html $(COVERAGE) /tmp/bisect-xenops*out

.PHONY: report
