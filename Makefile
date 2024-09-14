include config.mk

XAPIDOC=_build/install/default/usr/share/xapi/doc
XAPISDK=_build/install/default/usr/share/xapi/sdk
JOBS = $(shell getconf _NPROCESSORS_ONLN)
PROFILE=release
OPTMANDIR ?= $(OPTDIR)/man/man1/

.PHONY: build clean test doc python format install uninstall coverage

# if we have XAPI_VERSION set then set it in dune-project so we use that version number instead of the one obtained from git
# this is typically used when we're not building from a git repo
build:
	[ -z "${XAPI_VERSION}" ] || (sed -i '/(version.*)/d' dune-project && echo "(version ${XAPI_VERSION})" >> dune-project)
# if available use external file, otherwise use built-in, this allows building XAPI without being root
	! test -f $(SHAREDIR)/sm/XE_SR_ERRORCODES.xml || cp $(SHAREDIR)/sm/XE_SR_ERRORCODES.xml ocaml/sdk-gen/csharp/XE_SR_ERRORCODES.xml
	dune build @ocaml/idl/update-dm-lifecycle -j $(JOBS) --profile=$(PROFILE) --auto-promote || dune build @ocaml/idl/update-dm-lifecycle -j $(JOBS) --profile=$(PROFILE) --auto-promote
	dune build -j $(JOBS) --profile=$(PROFILE) @install @ocaml/xapi-storage/python/xapi/storage/api/v5/python @ocaml/xapi-doc @ocaml/sdk-gen/sdkgen

# Quickly verify that the code compiles, without actually building it
check:
	dune build @check -j $(JOBS)

coverage:
	dune runtest --instrument-with bisect_ppx --force --profile=$(RELEASE) -j $(JOBS)
	bisect-ppx-report html
	bisect-ppx-report summary --per-file

clean:
	dune clean

lint:
	dune build @python
	pylint --disable=line-too-long,too-few-public-methods,unused-argument,no-self-use,invalid-name,broad-except,protected-access,redefined-builtin,too-many-lines,wildcard-import,too-many-branches,too-many-arguments,unused-wildcard-import,raising-format-tuple,too-many-statements,duplicate-code _build/default/xapi-storage/python/xapi/storage/api/v5/*.py
	pycodestyle --ignore=E501 _build/default/xapi-storage/python/xapi/storage/api/v5/*.py


# ulimit -S -t <N>
#  Set a soft CPU time quota which will kill processes with SIGXCPU
#  This will in preference kill children of dune that consume CPU time (e.g. a stuck test),
#  and not dune itself (which should consume little CPU time when it just waits for subprocesses)
#  However it won't kill idle processes (e.g. a test sleeping and waiting for an event that never arrives)

# sleep <N> && ps
#   Prints a process tree once the timeout is reached to identify any sleeping processes that are stuck
#   (if we send 'dune' a SIGINT or SIGTERM it'd kill all subprocesses but won't say which ones were running and since when!)
#  -e: prints all processes
#  -ww: disables line length restrictions
#  -ly: prints additional columns, e.g. WCHAN which shows currently active syscall
#  -F: prints process start time and CPU time, useful in identifying which test got started recently,
#   and which one was running for a while
#  --forest prints a process tree

# timeout --foreground <M> dune
#  Sends a SIGTERM to dune after N seconds. This should print any pending buffered output, but won't tell us which processes were still running
#  The timeout used here should be > timeout in ulimit && ps to allow time for subprocesses to terminate first

# ulimit -n <NFILES>
#  By default the ulimit on some systems is very large (e.g. Fedora39 distrobox 1048576)
#  which causes some tests to take very long to run (e.g. forkexec tests which loop through and close all fds up to limit)

TEST_TIMEOUT=600
TEST_TIMEOUT2=1200
test:
	ulimit -S -t $(TEST_TIMEOUT); \
	ulimit -n 2048; \
	 (sleep $(TEST_TIMEOUT) && ps -ewwlyF --forest)& \
	 PSTREE_SLEEP_PID=$$!; \
	 trap "kill $${PSTREE_SLEEP_PID}" INT TERM EXIT; \
	 timeout --foreground $(TEST_TIMEOUT2) \
		 dune build --profile=$(PROFILE) --error-reporting=twice -j $(JOBS) @runtest @runtest-python

stresstest:
	dune build @stresstest --profile=$(PROFILE) --no-buffer -j $(JOBS)

# check that the IDL hash is current
schema:
	dune runtest ocaml/idl

doc:
	dune build --profile=$(PROFILE) @xapi-doc

sdk:
	dune build --profile=$(PROFILE)  @sdkgen xapi-sdk.install @ocaml/sdk-gen/install

.PHONY: sdk-build-c

sdk-build-c: sdk
	cd _build/install/default/share/c && make clean && make -j $(JOBS)

.PHONY: sdk-build-java

sdk-build-java: sdk
	cd _build/install/default/share/java && mvn -f xen-api/pom.xml -B clean package install -Drevision=0.0

python:
	$(MAKE) -C python3/examples build

doc-json: doc

format:
	dune build @fmt --auto-promote

.PHONY: quality-gate
quality-gate:
	./quality-gate.sh

.PHONY: install-scripts install-python3 install-dune1 install-dune2 install-dune3 install-dune4 install-extra

install-scripts:
	$(MAKE) -C scripts install
	
install-python3:
	$(MAKE) -C python3 install

install-parallel: install-dune1 install-dune2 install-dune3 install-dune4 install-scripts install-python3 install-extra

install-extra:
	mkdir -p $(DESTDIR)$(OPTDIR)/bin
	mkdir -p $(DESTDIR)$(OPTMANDIR)
	mkdir -p $(DESTDIR)$(LIBEXECDIR)
	mkdir -p $(DESTDIR)$(OPTDIR)/debug
	mkdir -p $(DESTDIR)/usr/bin
	mkdir -p $(DESTDIR)/usr/libexec/xapi
	mkdir -p $(DESTDIR)$(MANDIR)/man1
	mkdir -p $(DESTDIR)/etc
	mkdir -p $(DESTDIR)/etc/bash_completion.d
# ocaml/xapi
	scripts/install.sh 755 ocaml/quicktest/quicktest $(DESTDIR)$(OPTDIR)/debug
# ocaml/xe-cli
	ln -sf $(OPTDIR)/bin/xe $(DESTDIR)/usr/bin/xe
	scripts/install.sh 755 ocaml/xe-cli/bash-completion $(DESTDIR)/etc/bash_completion.d/xe
# rrd2csv
	scripts/install.sh 644 ocaml/rrd2csv/man/rrd2csv.1.man $(DESTDIR)$(OPTMANDIR)/rrd2csv.1
# rrdd-plugins
	install -D -m 644 ocaml/xcp-rrdd/bugtool-plugin/rrdd-plugins.xml $(DESTDIR)$(ETCXENDIR)/bugtool/xcp-rrdd-plugins.xml
	install -D -m 644 ocaml/xcp-rrdd/bugtool-plugin/rrdd-plugins/stuff.xml $(DESTDIR)$(ETCXENDIR)/bugtool/xcp-rrdd-plugins/stuff.xml
	install -D -m 755 ocaml/xcp-rrdd/bin/rrdp-scripts/sysconfig-rrdd-plugins $(DESTDIR)/etc/sysconfig/xcp-rrdd-plugins
	install -D -m 644 ocaml/xcp-rrdd/bin/rrdp-scripts/logrotate-rrdd-plugins $(DESTDIR)/etc/logrotate.d/xcp-rrdd-plugins
# vhd-tool
	install -m 644 ocaml/vhd-tool/cli/sparse_dd.conf           $(DESTDIR)/etc/sparse_dd.conf
# xenopsd
	install -D ./ocaml/xenopsd/scripts/xen-backend.rules $(DESTDIR)/$(ETCDIR)/udev/rules.d/xen-backend.rules
	install -D ./ocaml/xenopsd/scripts/qemu-wrapper $(DESTDIR)/$(QEMU_WRAPPER_DIR)/qemu-wrapper
	install -D ./ocaml/xenopsd/scripts/swtpm-wrapper $(DESTDIR)/$(QEMU_WRAPPER_DIR)/swtpm-wrapper
	install -D ./ocaml/xenopsd/scripts/pygrub-wrapper $(DESTDIR)/$(QEMU_WRAPPER_DIR)/pygrub-wrapper
	DESTDIR=$(DESTDIR) SBINDIR=$(SBINDIR) QEMU_WRAPPER_DIR=$(QEMU_WRAPPER_DIR) XENOPSD_LIBEXECDIR=$(XENOPSD_LIBEXECDIR) ETCDIR=$(ETCDIR) ./ocaml/xenopsd/scripts/make-custom-xenopsd.conf

install-dune1:
# dune can install libraries and several other files into the right locations
	dune install -j $(JOBS) --destdir=$(DESTDIR) --prefix=$(PREFIX) --libdir=$(LIBDIR) --mandir=$(MANDIR) \
		--libexecdir=$(XENOPSD_LIBEXECDIR) --datadir=$(SDKDIR) \
		xapi-client xapi-schema xapi-consts xapi-cli-protocol xapi-datamodel xapi-types \
		xen-api-client xen-api-client-lwt xen-api-client-async rrdd-plugin rrd-transport \
		gzip http-lib pciutil sexpr stunnel uuid xml-light2 zstd xapi-compression safe-resources \
		message-switch message-switch-async message-switch-cli message-switch-core message-switch-lwt \
		message-switch-unix xapi-idl forkexec xapi-forkexecd xapi-storage xapi-storage-script xapi-storage-cli \
		xapi-nbd varstored-guard xapi-log xapi-open-uri xapi-tracing xapi-tracing-export xapi-expiry-alerts cohttp-posix \
		xapi-rrd xapi-inventory clock xapi-rrdd rrddump xapi-rrd-transport-utils wsproxy xapi-networkd xapi-squeezed xapi-xenopsd-simulator xapi-xenopsd-cli xapi-xenopsd-xc xapi-sdk\
		xapi-stdext-date xapi-stdext-encodings xapi-stdext-pervasives xapi-stdext-std xapi-stdext-threads xapi-stdext-unix xapi-stdext-zerocheck

install-dune2:
	dune install -j $(JOBS) --destdir=$(DESTDIR) --prefix=$(OPTDIR) --libdir=$(LIBDIR) --mandir=$(MANDIR) --libexecdir=$(OPTDIR)/libexec --datadir=$(DOCDIR)  xapi xe rrdd-plugins

install-dune3:
	dune install -j $(JOBS) --destdir=$(DESTDIR) --prefix=$(OPTDIR) --libdir=$(LIBDIR) --mandir=$(MANDIR) --libexecdir=$(OPTDIR)/libexec --bindir=$(OPTDIR)/debug --datadir=$(OPTDIR)/debug xapi-debug

install-dune4:
	dune install -j $(JOBS) --destdir=$(DESTDIR) --prefix=$(PREFIX) --libdir=$(LIBDIR) --libexecdir=/usr/libexec --mandir=$(MANDIR) vhd-tool

install:
	$(MAKE) -j $(JOBS) install-parallel
# wsproxy
	mv $(DESTDIR)/usr/bin/wsproxy $(DESTDIR)$(LIBEXECDIR)/wsproxy
	(cd $(DESTDIR)/$(XENOPSD_LIBEXECDIR) && ln -sf pvs-proxy-ovs-setup setup-pvs-proxy-rules)
	chmod +x $(DESTDIR)$(DOCDIR)/doc-convert.sh
	# backward compat with existing specfile, to be removed after it is updated
	find $(DESTDIR) -name '*.cmxs' -delete
	for pkg in rrdd-plugins xapi-debug xapi xe xapi-networkd xapi-xenopsd-cli xapi-xenopsd-simulator xapi-xenopsd-xc xapi-squeezed xapi-rrdd xapi-rrd-transport-utils rrddump wsproxy xapi-sdk vhd-tool; do for f in CHANGELOG LICENSE README.markdown; do rm $(DESTDIR)$(OPTDIR)/doc/$$pkg/$$f $(DESTDIR)$(PREFIX)/doc/$$pkg/$$f -f; done; for f in META dune-package opam; do rm $(DESTDIR)$(LIBDIR)/$$pkg/$$f -f; done; done;


uninstall:
	# only removes what was installed with `dune install`
	dune uninstall --destdir=$(DESTDIR) --prefix=$(PREFIX) --libdir=$(LIBDIR) --mandir=$(MANDIR) \
		xapi-client xapi-schema xapi-consts xapi-cli-protocol xapi-datamodel xapi-types \
		xen-api-client xen-api-client-lwt xen-api-client-async rrdd-plugin rrd-transport \
		gzip http-lib pciutil sexpr stunnel uuid xml-light2 zstd xapi-compression safe-resources \
		message-switch message-switch-async message-switch-cli message-switch-core message-switch-lwt \
		message-switch-unix xapi-idl forkexec xapi-forkexecd xapi-storage xapi-storage-script xapi-log \
		xapi-open-uri xapi-tracing xapi-tracing-export xapi-expiry-alerts cohttp-posix \
		xapi-rrd xapi-inventory clock xapi-rrdd rrddump xapi-rrd-transport-utils wsproxy xapi-networkd xapi-squeezed xapi-xenopsd-simulator xapi-xenopsd-cli\
		xapi-stdext-date xapi-stdext-encodings xapi-stdext-pervasives xapi-stdext-std xapi-stdext-threads xapi-stdext-unix xapi-stdext-zerocheck
	dune uninstall --destdir=$(DESTDIR) --prefix=$(OPTDIR) --libdir=$(LIBDIR) --mandir=$(MANDIR)  xapi xe rrdd-plugins
	dune uninstall --destdir=$(DESTDIR) --prefix=$(PREFIX) --libdir=$(LIBDIR) --mandir=$(MANDIR) --libexecdir=$(XENOPSD_LIBEXECDIR)  xapi-xenopsd-xc
	dune uninstall --destdir=$(DESTDIR) --prefix=$(OPTDIR) --libdir=$(LIBDIR) --mandir=$(MANDIR) --libexecdir=$(OPTDIR)/libexec --bindir=$(OPTDIR)/debug --datadir=$(OPTDIR)/debug xapi-debug

compile_flags.txt: Makefile
	(ocamlc -config-var ocamlc_cflags;\
	ocamlc -config-var ocamlc_cppflags;\
	echo -I$(shell ocamlc -where);\
	echo -Wall -Wextra -Wstrict-prototypes -D_FORTIFY_SOURCE=2\
	) | xargs -n1 echo >$@
