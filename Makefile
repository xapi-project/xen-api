include config.mk

XAPIDOC=_build/install/default/xapi/doc
XAPISDK=_build/install/default/xapi/sdk
JOBS = $(shell getconf _NPROCESSORS_ONLN)
PROFILE=release
OPTMANDIR ?= $(OPTDIR)/man/man1/

.PHONY: build clean test doc python format install uninstall

# if we have XAPI_VERSION set then set it in dune-project so we use that version number instead of the one obtained from git
# this is typically used when we're not building from a git repo
build:
	[ -z "${XAPI_VERSION}" ] || (sed -i '/(version.*)/d' dune-project && echo "(version ${XAPI_VERSION})" >> dune-project)
	dune build @update-dm-lifecycle -j $(JOBS) --profile=$(PROFILE) --auto-promote || dune build @update-dm-lifecycle -j $(JOBS) --profile=$(PROFILE) --auto-promote
	dune build @install -j $(JOBS) --profile=$(PROFILE)
	dune build @python --profile=$(PROFILE)

# Quickly verify that the code compiles, without actually building it
check:
	dune build @check -j $(JOBS)

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
	 (sleep $(TEST_TIMEOUT) && ps -ewwlyF --forest)& \
	 PSTREE_SLEEP_PID=$$!; \
	 trap "kill $${PSTREE_SLEEP_PID}" SIGINT SIGTERM EXIT; \
	 timeout --foreground $(TEST_TIMEOUT2) \
		 dune runtest --profile=$(PROFILE) --error-reporting=twice -j $(JOBS)
ifneq ($(PY_TEST), NO)
	dune build @runtest-python --profile=$(PROFILE)
endif

stresstest:
	dune build @stresstest --profile=$(PROFILE) --no-buffer -j $(JOBS)

# check that the IDL hash is current
schema:
	dune runtest ocaml/idl

doc:
	dune build --profile=$(PROFILE) ocaml/idl/datamodel_main.exe
	dune build --profile=$(PROFILE) -f @ocaml/doc/jsapigen
	mkdir -p $(XAPIDOC)/html
	cp -r _build/default/ocaml/doc/api $(XAPIDOC)/html
	cp _build/default/ocaml/doc/branding.js $(XAPIDOC)/html
	cp ocaml/doc/*.js ocaml/doc/*.html ocaml/doc/*.css $(XAPIDOC)/html
	dune exec --profile=$(PROFILE) -- ocaml/idl/datamodel_main.exe -closed -markdown $(XAPIDOC)/markdown
	cp ocaml/doc/*.dot ocaml/doc/doc-convert.sh $(XAPIDOC)
	find ocaml/doc -name "*.md" -not -name "README.md" -exec cp {} $(XAPIDOC)/markdown/ \;
# Build manpages, networkd generated these
	dune build --profile=$(PROFILE) -f @man

sdk:
	cp $(SHAREDIR)/sm/XE_SR_ERRORCODES.xml ocaml/sdk-gen/csharp/XE_SR_ERRORCODES.xml
	dune build --profile=$(PROFILE) \
		ocaml/sdk-gen/c/gen_c_binding.exe \
		ocaml/sdk-gen/csharp/gen_csharp_binding.exe \
		ocaml/sdk-gen/java/main.exe \
		ocaml/sdk-gen/powershell/gen_powershell_binding.exe
	dune build --profile=$(PROFILE) -f\
		@ocaml/sdk-gen/c/generate \
		@ocaml/sdk-gen/csharp/generate \
		@ocaml/sdk-gen/java/generate \
		@ocaml/sdk-gen/powershell/generate
	rm -rf $(XAPISDK)
	mkdir -p $(XAPISDK)/c
	mkdir -p $(XAPISDK)/csharp
	mkdir -p $(XAPISDK)/java
	mkdir -p $(XAPISDK)/powershell
	mkdir -p $(XAPISDK)/python
	cp -r _build/default/ocaml/sdk-gen/c/autogen/* $(XAPISDK)/c
	cp -r _build/default/ocaml/sdk-gen/csharp/autogen/* $(XAPISDK)/csharp
	cp -r _build/default/ocaml/sdk-gen/java/autogen/* $(XAPISDK)/java
	cp -r _build/default/ocaml/sdk-gen/powershell/autogen/* $(XAPISDK)/powershell
	cp scripts/examples/python/XenAPI/XenAPI.py $(XAPISDK)/python
	sh ocaml/sdk-gen/windows-line-endings.sh $(XAPISDK)/csharp
	sh ocaml/sdk-gen/windows-line-endings.sh $(XAPISDK)/powershell

# workaround for no .resx generation, just for compilation testing
sdksanity: sdk
	sed -i 's/FriendlyErrorNames.ResourceManager/null/g' ./_build/install/default/xapi/sdk/csharp/src/Failure.cs
	cd _build/install/default/xapi/sdk/csharp/src && dotnet add package Newtonsoft.Json && dotnet build -f netstandard2.0

.PHONY: sdk-build-java

sdk-build-java: sdk
	cd _build/install/default/xapi/sdk/java && mvn -f xen-api/pom.xml -B clean package install -Drevision=0.0

python:
	$(MAKE) -C scripts/examples/python build

doc-json:
	dune exec --profile=$(PROFILE) -- ocaml/idl/json_backend/gen_json.exe -destdir $(XAPIDOC)/jekyll

format:
	dune build @fmt --auto-promote

.PHONY: quality-gate
quality-gate:
	./quality-gate.sh

install: build doc sdk doc-json
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
	make -C scripts install
	make -C python3 install
	cp -f _build/install/default/bin/xapi $(DESTDIR)$(OPTDIR)/bin/xapi
	scripts/install.sh 755 ocaml/quicktest/quicktest $(DESTDIR)$(OPTDIR)/debug
	cp -f _build/install/default/bin/quicktestbin $(DESTDIR)$(OPTDIR)/debug/quicktestbin
	scripts/install.sh 644 _build/install/default/share/xapi/rbac_static.csv $(DESTDIR)$(OPTDIR)/debug
# ocaml/xsh
	cp -f _build/install/default/bin/xsh $(DESTDIR)$(OPTDIR)/bin/xsh
# ocaml/xe-cli
	scripts/install.sh 755 _build/install/default/bin/xe $(DESTDIR)$(OPTDIR)/bin/xe
	ln -sf $(OPTDIR)/bin/xe $(DESTDIR)/usr/bin/xe
	scripts/install.sh 755 ocaml/xe-cli/bash-completion $(DESTDIR)/etc/bash_completion.d/xe
# ocaml/vncproxy
	scripts/install.sh 755 _build/install/default/bin/vncproxy $(DESTDIR)$(OPTDIR)/debug/vncproxy
# ocaml/perftest
	scripts/install.sh 755 _build/install/default/bin/perftest $(DESTDIR)$(OPTDIR)/debug/perftest
# ocaml/suspend-image-viewer
	scripts/install.sh 755 _build/install/default/bin/suspend-image-viewer $(DESTDIR)$(OPTDIR)/debug/suspend-image-viewer
# ocaml/mpathalert
	scripts/install.sh 755 _build/install/default/bin/mpathalert $(DESTDIR)$(OPTDIR)/bin/mpathalert
# ocaml/license
	scripts/install.sh 755 _build/install/default/bin/daily-license-check $(DESTDIR)$(LIBEXECDIR)/daily-license-check
# ocaml/alerts/certificate
	scripts/install.sh 755 _build/install/default/bin/alert-certificate-check $(DESTDIR)$(LIBEXECDIR)/alert-certificate-check
# ocaml/events
	scripts/install.sh 755 _build/install/default/bin/event_listen $(DESTDIR)$(OPTDIR)/debug/event_listen
# ocaml/db_process
	scripts/install.sh 755 _build/install/default/bin/xapi-db-process $(DESTDIR)$(OPTDIR)/bin/xapi-db-process
# ocaml/cdrommon
	scripts/install.sh 755 _build/install/default/bin/cdrommon $(DESTDIR)$(LIBEXECDIR)/cdrommon
# ocaml/database
	scripts/install.sh 755 _build/install/default/bin/block_device_io $(DESTDIR)$(LIBEXECDIR)/block_device_io
# ocaml/gencert
	scripts/install.sh 755 _build/install/default/bin/gencert $(DESTDIR)$(LIBEXECDIR)/gencert
# ocaml/rrd2csv
	scripts/install.sh 755 _build/install/default/bin/rrd2csv $(DESTDIR)$(OPTDIR)/bin/rrd2csv
	scripts/install.sh 644 ocaml/rrd2csv/man/rrd2csv.1.man $(DESTDIR)$(OPTMANDIR)/rrd2csv.1
# ocaml/xs-trace
	scripts/install.sh 755 _build/install/default/bin/xs-trace $(DESTDIR)/usr/bin/xs-trace
# xcp-rrdd
	install -D _build/install/default/bin/xcp-rrdd $(DESTDIR)/usr/sbin/xcp-rrdd
	install -D _build/install/default/bin/rrddump $(DESTDIR)/usr/bin/rrddump
# rrd-cli
	install -D _build/install/default/bin/rrd-cli $(DESTDIR)/usr/bin/rrd-cli
# rrd-transport
	install -D _build/install/default/bin/rrdreader $(DESTDIR)/usr/bin/rrdreader
	install -D _build/install/default/bin/rrdwriter $(DESTDIR)/usr/bin/rrdwriter
# rrdd-plugins
	install -D -m 755 _build/install/default/bin/xcp-rrdd-iostat $(DESTDIR)$(LIBEXECDIR)/xcp-rrdd-plugins/xcp-rrdd-iostat
	install -D -m 755 _build/install/default/bin/xcp-rrdd-squeezed $(DESTDIR)$(LIBEXECDIR)/xcp-rrdd-plugins/xcp-rrdd-squeezed
	install -D -m 755 _build/install/default/bin/xcp-rrdd-xenpm $(DESTDIR)$(LIBEXECDIR)/xcp-rrdd-plugins/xcp-rrdd-xenpm
	install -D -m 644 ocaml/xcp-rrdd/bugtool-plugin/rrdd-plugins.xml $(DESTDIR)$(ETCXENDIR)/bugtool/xcp-rrdd-plugins.xml
	install -D -m 644 ocaml/xcp-rrdd/bugtool-plugin/rrdd-plugins/stuff.xml $(DESTDIR)$(ETCXENDIR)/bugtool/xcp-rrdd-plugins/stuff.xml
	install -D -m 755 ocaml/xcp-rrdd/bin/rrdp-scripts/sysconfig-rrdd-plugins $(DESTDIR)/etc/sysconfig/xcp-rrdd-plugins
	install -D -m 644 ocaml/xcp-rrdd/bin/rrdp-scripts/logrotate-rrdd-plugins $(DESTDIR)/etc/logrotate.d/xcp-rrdd-plugins
# vhd-tool
	install -m 755 _build/install/default/bin/sparse_dd        $(DESTDIR)/usr/libexec/xapi/sparse_dd
	install -m 755 _build/install/default/bin/vhd-tool         $(DESTDIR)/usr/bin/vhd-tool
	install -m 644 ocaml/vhd-tool/cli/sparse_dd.conf           $(DESTDIR)/etc/sparse_dd.conf
	install -m 755 _build/install/default/bin/get_vhd_vsize    $(DESTDIR)/usr/libexec/xapi/get_vhd_vsize
	install -m 755 ocaml/vhd-tool/scripts/get_nbd_extents.py   $(DESTDIR)$(LIBEXECDIR)/get_nbd_extents.py
	install -m 644 ocaml/vhd-tool/scripts/python_nbd_client.py $(DESTDIR)$(LIBEXECDIR)/python_nbd_client.py
# xenopsd
	install -D _build/install/default/bin/xenopsd-simulator $(DESTDIR)/$(SBINDIR)/xenopsd-simulator
	install -D _build/install/default/man/man1/xenopsd-simulator.1.gz $(DESTDIR)/$(MANDIR)/man1/xenopsd-simulator.1.gz
	install -D _build/install/default/bin/xenopsd-xc $(DESTDIR)/$(SBINDIR)/xenopsd-xc
	install -D _build/install/default/bin/fence.bin $(DESTDIR)/$(LIBEXECDIR)/fence.bin
	install -D _build/install/default/man/man1/xenopsd-xc.1.gz $(DESTDIR)/$(MANDIR)/man1/xenopsd-xc.1.gz
	install -D _build/install/default/bin/set-domain-uuid $(DESTDIR)/$(XENOPSD_LIBEXECDIR)/set-domain-uuid
	install -D _build/install/default/bin/xenops-cli $(DESTDIR)/$(SBINDIR)/xenops-cli
	install -D _build/install/default/man/man1/xenops-cli.1.gz $(DESTDIR)/$(MANDIR)/man1/xenops-cli.1.gz
	install -D _build/install/default/bin/list_domains $(DESTDIR)/$(BINDIR)/list_domains
	install -D ./ocaml/xenopsd/scripts/vif $(DESTDIR)/$(XENOPSD_LIBEXECDIR)/vif
	install -D ./ocaml/xenopsd/scripts/vif-real $(DESTDIR)/$(XENOPSD_LIBEXECDIR)/vif-real
	install -D ./ocaml/xenopsd/scripts/block $(DESTDIR)/$(XENOPSD_LIBEXECDIR)/block
	install -D ./ocaml/xenopsd/scripts/xen-backend.rules $(DESTDIR)/$(ETCDIR)/udev/rules.d/xen-backend.rules
	install -D ./ocaml/xenopsd/scripts/tap $(DESTDIR)/$(XENOPSD_LIBEXECDIR)/tap
	install -D ./ocaml/xenopsd/scripts/qemu-vif-script $(DESTDIR)/$(XENOPSD_LIBEXECDIR)/qemu-vif-script
	install -D ./ocaml/xenopsd/scripts/setup-vif-rules $(DESTDIR)/$(XENOPSD_LIBEXECDIR)/setup-vif-rules
	install -D ./_build/install/default/bin/pvs-proxy-ovs-setup $(DESTDIR)/$(XENOPSD_LIBEXECDIR)/pvs-proxy-ovs-setup
	(cd $(DESTDIR)/$(XENOPSD_LIBEXECDIR) && ln -s pvs-proxy-ovs-setup setup-pvs-proxy-rules)
	install -D ./ocaml/xenopsd/scripts/common.py $(DESTDIR)/$(XENOPSD_LIBEXECDIR)/common.py
	install -D ./ocaml/xenopsd/scripts/igmp_query_injector.py $(DESTDIR)/$(XENOPSD_LIBEXECDIR)/igmp_query_injector.py
	install -D ./ocaml/xenopsd/scripts/qemu-wrapper $(DESTDIR)/$(QEMU_WRAPPER_DIR)/qemu-wrapper
	install -D ./ocaml/xenopsd/scripts/swtpm-wrapper $(DESTDIR)/$(QEMU_WRAPPER_DIR)/swtpm-wrapper
	install -D ./ocaml/xenopsd/scripts/pygrub-wrapper $(DESTDIR)/$(QEMU_WRAPPER_DIR)/pygrub-wrapper
	DESTDIR=$(DESTDIR) SBINDIR=$(SBINDIR) QEMU_WRAPPER_DIR=$(QEMU_WRAPPER_DIR) XENOPSD_LIBEXECDIR=$(XENOPSD_LIBEXECDIR) ETCDIR=$(ETCDIR) ./ocaml/xenopsd/scripts/make-custom-xenopsd.conf
# squeezed
	install -D _build/install/default/bin/squeezed $(DESTDIR)/$(SBINDIR)/squeezed
# xcp-networkd
	install -m 755 _build/install/default/bin/xapi-networkd         $(DESTDIR)/usr/sbin/xcp-networkd
	install -m 755 _build/install/default/bin/networkd_db           $(DESTDIR)/usr/bin/networkd_db
	install -m 644 _build/default/ocaml/networkd/bin/xcp-networkd.1 $(DESTDIR)/usr/share/man/man1/xcp-networkd.1
# wsproxy
	install -m 755 _build/install/default/bin/wsproxy $(DESTDIR)$(LIBEXECDIR)/wsproxy
# dune can install libraries and several other files into the right locations
	dune install --destdir=$(DESTDIR) --prefix=$(PREFIX) --libdir=$(LIBDIR) --mandir=$(MANDIR) \
		xapi-client xapi-schema xapi-consts xapi-cli-protocol xapi-datamodel xapi-types \
		xen-api-client xen-api-client-lwt xen-api-client-async rrdd-plugin rrd-transport \
		gzip http-lib pciutil sexpr stunnel uuid xml-light2 zstd xapi-compression safe-resources \
		message-switch message-switch-async message-switch-cli message-switch-core message-switch-lwt \
		message-switch-unix xapi-idl forkexec xapi-forkexecd xapi-storage xapi-storage-script xapi-storage-cli \
		xapi-nbd varstored-guard xapi-log xapi-open-uri xapi-tracing xapi-expiry-alerts cohttp-posix \
		xapi-rrd xapi-inventory \
		xapi-stdext-date xapi-stdext-encodings xapi-stdext-pervasives xapi-stdext-std xapi-stdext-threads xapi-stdext-unix xapi-stdext-zerocheck xapi-stdext
# docs
	mkdir -p $(DESTDIR)$(DOCDIR)
	cp -r $(XAPIDOC)/jekyll $(DESTDIR)$(DOCDIR)
	cp -r $(XAPIDOC)/html $(DESTDIR)$(DOCDIR)
	cp -r $(XAPIDOC)/markdown $(DESTDIR)$(DOCDIR)
	cp $(XAPIDOC)/*.dot $(XAPIDOC)/doc-convert.sh $(DESTDIR)$(DOCDIR)
# sdk
	mkdir -p $(DESTDIR)$(SDKDIR)
	cp -r $(XAPISDK)/* $(DESTDIR)$(SDKDIR)
	find $(DESTDIR)$(SDKDIR) -type f -exec chmod 644 {} \;

uninstall:
	# only removes what was installed with `dune install`
	dune uninstall --destdir=$(DESTDIR) --prefix=$(PREFIX) --libdir=$(LIBDIR) --mandir=$(MANDIR) \
		xapi-client xapi-schema xapi-consts xapi-cli-protocol xapi-datamodel xapi-types \
		xen-api-client xen-api-client-lwt xen-api-client-async rrdd-plugin rrd-transport \
		gzip http-lib pciutil sexpr stunnel uuid xml-light2 zstd xapi-compression safe-resources \
		message-switch message-switch-async message-switch-cli message-switch-core message-switch-lwt \
		message-switch-unix xapi-idl forkexec xapi-forkexecd xapi-storage xapi-storage-script xapi-log \
		xapi-open-uri xapi-tracing xapi-expiry-alerts cohttp-posix \
		xapi-rrd xapi-inventory \
		xapi-stdext-date xapi-stdext-encodings xapi-stdext-pervasives xapi-stdext-std xapi-stdext-threads xapi-stdext-unix xapi-stdext-zerocheck xapi-stdext

compile_flags.txt: Makefile
	(ocamlc -config-var ocamlc_cflags;\
	ocamlc -config-var ocamlc_cppflags;\
	echo -I$(shell ocamlc -where);\
	echo -Wall -Wextra -Wstrict-prototypes -D_FORTIFY_SOURCE=2\
	) | xargs -n1 echo >$@
