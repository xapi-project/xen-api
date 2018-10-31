include config.mk

OPAM_PREFIX=$(DESTDIR)$(shell opam config var prefix)
OPAM_LIBDIR=$(DESTDIR)$(shell opam config var lib)
XAPIDOC=_build/install/default/xapi/doc

.PHONY: build clean test doc reindent install uninstall

build:
	jbuilder build @install -j $$(getconf _NPROCESSORS_ONLN)

clean:
	jbuilder clean

test:
	jbuilder runtest --no-buffer -j $$(getconf _NPROCESSORS_ONLN)

doc:
	jbuilder build ocaml/idl/datamodel_main.exe
	jbuilder build -f @ocaml/doc/jsapigen
	mkdir -p $(XAPIDOC)/html
	cp -r _build/default/ocaml/doc/api $(XAPIDOC)/html
	cp _build/default/ocaml/doc/branding.js $(XAPIDOC)/html
	cp ocaml/doc/*.js ocaml/doc/*.html ocaml/doc/*.css $(XAPIDOC)/html
	jbuilder exec -- ocaml/idl/datamodel_main.exe -closed -markdown $(XAPIDOC)/markdown
	cp ocaml/doc/*.dot ocaml/doc/doc-convert.sh $(XAPIDOC)
	find ocaml/doc -name "*.md" -not -name "README.md" -exec cp {} $(XAPIDOC)/markdown/ \;

doc-json:
	jbuilder build ocaml/idl/json_backend/gen_json.exe
	jbuilder exec -- ocaml/idl/json_backend/gen_json.exe -destdir _build/install/default/jekyll

reindent:
	git ls-files '*.ml*' '**/*.ml*' | xargs ocp-indent --syntax cstruct -i

install: build doc
	mkdir -p $(DESTDIR)$(SBINDIR)
	mkdir -p $(DESTDIR)$(OPTDIR)/bin
	mkdir -p $(DESTDIR)$(LIBEXECDIR)
	mkdir -p $(DESTDIR)$(OPTDIR)/debug
	mkdir -p $(DESTDIR)/usr/bin
	mkdir -p $(DESTDIR)/etc/bash_completion.d
	mkdir -p $(OPAM_LIBDIR)
# ocaml/xapi
	make -C scripts install
	cp -f _build/install/default/bin/xapi $(DESTDIR)$(SBINDIR)/xapi
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
# ocaml/ptoken
	scripts/install.sh 755 _build/install/default/bin/genptoken $(DESTDIR)$(LIBEXECDIR)/genptoken
# ocaml/perftest
	scripts/install.sh 755 _build/install/default/bin/perftest $(DESTDIR)$(OPTDIR)/debug/perftest
# ocaml/mpathalert
	scripts/install.sh 755 _build/install/default/bin/mpathalert $(DESTDIR)$(OPTDIR)/bin/mpathalert
# ocaml/license
	scripts/install.sh 755 _build/install/default/bin/daily-license-check $(DESTDIR)$(LIBEXECDIR)/daily-license-check
# ocaml/events
	scripts/install.sh 755 _build/install/default/bin/event_listen $(DESTDIR)$(OPTDIR)/debug/event_listen
# ocaml/db_process
	scripts/install.sh 755 _build/install/default/bin/xapi-db-process $(DESTDIR)$(OPTDIR)/bin/xapi-db-process
# ocaml/cdrommon
	scripts/install.sh 755 _build/install/default/bin/cdrommon $(DESTDIR)$(LIBEXECDIR)/cdrommon
# ocaml/database
	scripts/install.sh 755 _build/install/default/bin/block_device_io $(DESTDIR)$(LIBEXECDIR)/block_device_io
# Libraries
	jbuilder install --prefix=$(OPAM_PREFIX) --libdir=$(OPAM_LIBDIR) xapi-client xapi-database xapi-consts xapi-cli-protocol xapi-datamodel xapi-types
# docs
	mkdir -p $(DESTDIR)$(DOCDIR)
	cp -r $(XAPIDOC)/html $(DESTDIR)$(DOCDIR)
	cp -r $(XAPIDOC)/markdown $(DESTDIR)$(DOCDIR)
	cp $(XAPIDOC)/*.dot $(XAPIDOC)/doc-convert.sh $(DESTDIR)$(DOCDIR)

uninstall:
	# only removes the libraries, which were installed with `jbuilder install`
	jbuilder uninstall --prefix=$(OPAM_PREFIX) --libdir=$(OPAM_LIBDIR) xapi-client xapi-database xapi-consts xapi-cli-protocol xapi-datamodel xapi-types
