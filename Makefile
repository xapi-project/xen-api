include config.mk
INSTALL_PATH = $(DESTDIR)/$(shell ocamlfind printconf destdir)
OCAMLFIND_DESTDIR = $(INSTALL_PATH)
CONFIGUREFLAGS=--enable-tests --override ocamlbuildflags '-j 8'

export OCAMLFIND_DESTDIR

SETUP = ocaml setup.ml

build: setup.data
	$(SETUP) -build $(BUILDFLAGS)

doc: setup.data build
	$(SETUP) -doc $(DOCFLAGS)
	./jsapi.native -destdir _build/ocaml/doc -templdir ocaml/doc/templates

test: setup.data build
	$(SETUP) -test $(TESTFLAGS)

all: setup.ml
	$(SETUP) -all $(ALLFLAGS)

uninstall: setup.data
	$(SETUP) -uninstall $(UNINSTALLFLAGS)

reinstall: setup.data
	$(SETUP) -reinstall $(REINSTALLFLAGS)

clean: setup.ml
	$(SETUP) -clean $(CLEANFLAGS)

distclean: setup.ml
	$(SETUP) -distclean $(DISTCLEANFLAGS)

setup.data: setup.ml
	$(SETUP) -configure $(CONFIGUREFLAGS)

setup.ml: _oasis.in
ifeq ($(BISECT_COVERAGE),YES)
	rm -f _oasis
	sed -e 's/BuildDepends:/BuildDepends: bisect_ppx,/' _oasis.in >_oasis
	ln -sf profiling-enabled/coverage.ml ocaml/xapi/coverage.ml
else
	ln -sf _oasis.in _oasis
	ln -sf profiling-disabled/coverage.ml ocaml/xapi/coverage.ml
endif
	oasis setup -setup-update dynamic

configure: setup.ml
	$(SETUP) -configure $(CONFIGUREFLAGS)

.PHONY: build doc test all install uninstall reinstall clean distclean configure

rbac_static.csv: gen_api_main.native
	./gen_api_main.native -mode rbac -gendebug -output rbac_static.csv

doc-json:
	./gen_json.native

install: setup.data rbac_static.csv
	mkdir -p $(DESTDIR)$(SBINDIR)
	mkdir -p $(DESTDIR)$(OPTDIR)/bin
	mkdir -p $(DESTDIR)$(LIBEXECDIR)
	mkdir -p $(DESTDIR)$(OPTDIR)/debug
	mkdir -p $(DESTDIR)/usr/bin
	mkdir -p $(DESTDIR)/etc/bash_completion.d
	mkdir -p $(INSTALL_PATH)
# ocaml/xapi
	make -C scripts install
	cp -f xapi_main.native $(DESTDIR)$(SBINDIR)/xapi
	scripts/install.sh 755 ocaml/xapi/quicktest $(DESTDIR)$(OPTDIR)/debug
	cp -f quicktest.native $(DESTDIR)$(OPTDIR)/debug/quicktestbin
	scripts/install.sh 644 rbac_static.csv $(DESTDIR)$(OPTDIR)/debug
# ocaml/xsh
	cp -f xsh.native $(DESTDIR)$(OPTDIR)/bin/xsh
# ocaml/xe-cli
	scripts/install.sh 755 newcli.native $(DESTDIR)$(OPTDIR)/bin/xe
	ln -sf $(OPTDIR)/bin/xe $(DESTDIR)/usr/bin/xe
	scripts/install.sh 755 ocaml/xe-cli/bash-completion $(DESTDIR)/etc/bash_completion.d/xe
# ocaml/vncproxy
	scripts/install.sh 755 vncproxy.native $(DESTDIR)$(OPTDIR)/debug/vncproxy
# ocaml/ptoken
	scripts/install.sh 755 genptoken.native $(DESTDIR)$(LIBEXECDIR)/genptoken
# ocaml/perftest
	scripts/install.sh 755 perftest.native $(DESTDIR)$(OPTDIR)/debug/perftest
# ocaml/mpathalert
	scripts/install.sh 755 mpathalert.native $(DESTDIR)$(OPTDIR)/bin/mpathalert
# ocaml/license
	scripts/install.sh 755 daily_license_check_main.native $(DESTDIR)$(LIBEXECDIR)/daily-license-check
# ocaml/graph
	scripts/install.sh 755 graph.native $(DESTDIR)$(OPTDIR)/debug/graph
# ocaml/events
	scripts/install.sh 755 event_listen.native $(DESTDIR)$(OPTDIR)/debug/event_listen
# ocaml/db_process
	scripts/install.sh 755 xapi_db_process.native $(DESTDIR)$(OPTDIR)/bin/xapi-db-process
# ocaml/cdrommon
	scripts/install.sh 755 cdrommon.native $(DESTDIR)$(LIBEXECDIR)/cdrommon
# ocaml/database
	scripts/install.sh 755 block_device_io.native $(DESTDIR)$(LIBEXECDIR)/block_device_io
# Libraries
	ocaml setup.ml -install
# xenserver html docs
	mkdir -p $(DESTDIR)$(DOCDIR)/html/xenserver
	cp -r -L _build/ocaml/doc/api $(DESTDIR)$(DOCDIR)/html/xenserver
	cd ocaml/doc && cp *.js *.html *.css *.png $(DESTDIR)$(DOCDIR)/html/xenserver
	cp _build/ocaml/doc/branding.js $(DESTDIR)$(DOCDIR)/html/xenserver/branding.js
