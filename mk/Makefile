# makefile for api component in build system
USE_BRANDING := yes
USE_BUILD_NUMBER := yes
IMPORT_BRANDING := yes
include $(B_BASE)/common.mk
include $(B_BASE)/rpmbuild.mk

OUTPUT_PKG := $(MY_MAIN_PACKAGES)/xapi.tar.bz2
OUTPUT_XAPI := $(MY_OUTPUT_DIR)/xapi
OUTPUT_CLI := $(MY_OUTPUT_DIR)/xe
OUTPUT_CLI_RT := $(MY_OUTPUT_DIR)/cli-regress.tar.gz
OUTPUT_CLI_RT_GENEVA := $(MY_OUTPUT_DIR)/cli-regress-geneva.tar.gz
OUTPUT_FAKESERVER := $(MY_OUTPUT_DIR)/fakeserver.tar.bz2
OUTPUT_DOCS := $(MY_OUTPUT_DIR)/sdkiso
OUTPUT_WEBZIP := $(MY_OUTPUT_DIR)/sdk-html.zip
OUTPUT_SRC := $(MY_OUTPUT_DIR)/xen-api-src.tar.bz2
OUTPUT_PATCH := $(MY_OUTPUT_DIR)/testpatch.asc
OUTPUT_DATAMODEL_DIR := $(MY_OUTPUT_DIR)/datamodel

CLI_RPM_SOURCES :=
CLI_RPM_SOURCES += $(RPM_SOURCESDIR)/xe
CLI_RPM_SOURCES += $(RPM_SOURCESDIR)/bash-completion

JQUERY_PACK_DIST=$(CARBON_DISTFILES)/javascript/jquery/jquery-1.1.3.1.pack.js
JQUERY_TV_DIST=$(CARBON_DISTFILES)/javascript/jquery/treeview/jquery.treeview.zip

export BUILD_NUMBER
export PRODUCT_BRAND
export PRODUCT_VERSION

REPO=$(call hg_loc,xen-api)

.PHONY: build
build: $(OUTPUT_PKG) $(MY_OBJ_DIR)/.rpmbuild.cli.stamp $(MY_SOURCES)/MANIFEST
	$(call mkdir_clean,$(MY_LINUX_CDFILES)/client_install)
	install -m 755 -o root -g root $(RPM_RPMSDIR)/i386/xe-cli-*.rpm $(MY_LINUX_CDFILES)/client_install/

$(MY_SOURCES)/MANIFEST: $(MY_SOURCES_DIRSTAMP) $(OUTPUT_SRC)
	echo api gpl+lgpl-with-linking-exception file $(OUTPUT_SRC) > $@
	echo api gpl file $(JQUERY_PACK_DIST) >> $@
	echo api gpl file $(JQUERY_TV_DIST) >> $@

$(OUTPUT_SRC):
	cd $(REPO) && hg archive -t tbz2 $(HG_EXCLUDE) $@

$(OUTPUT_PKG) $(OUTPUT_FAKESERVER) $(OUTPUT_XAPI) $(OUTPUT_CLI): $(call hg_req,xen-api) $(MY_MAIN_PACKAGES)/.dirstamp
	rm -rf $(REPO)/dist
	$(MAKE) clean
	$(MAKE) -C $(REPO)
	install -m 755 -o root -g root $(REPO)/ocaml/p2v/p2v $(MY_OUTPUT_DIR)/p2v-server
	install -m 755 -o root -g root $(REPO)/ocaml/p2v/closeandexec_static $(MY_OUTPUT_DIR)/closeandexec_static
	$(MAKE) -C $(REPO) install
	mkdir -p $(REPO)/dist/staging/var/xapi
	mkdir -p $(REPO)/dist/staging/var/log/xen
	mkdir -p $(REPO)/dist/staging/var/opt/xen/iso_import
	cp $(REPO)/dist/staging/opt/xensource/bin/xapi $(OUTPUT_XAPI)
	tar -C $(REPO)/dist/staging -jcf $(OUTPUT_PKG) .
	tar -C $(REPO)/dist/cli-rt -zcf $(OUTPUT_CLI_RT) .
	tar -C $(REPO)/dist/cli-regress-geneva -zcf $(OUTPUT_CLI_RT_GENEVA) .
	install -m 755 -o root -g root $(REPO)/dist/staging/opt/xensource/bin/xe $(MY_OUTPUT_DIR)/xe
	rm -rf $(REPO)/dist
	$(MAKE) -C $(REPO) sdk-install
	$(call mkdir_clean,$(OUTPUT_DOCS))
	cp $(REPO)/dist/staging/SDK/README.html $(OUTPUT_DOCS)/
	cp -a $(REPO)/dist/staging/SDK/docs $(OUTPUT_DOCS)/
	cd $(REPO)/dist/staging/SDKwww && zip -9rv $(OUTPUT_WEBZIP) .
	tar -C $(REPO)/dist/staging --exclude=etc/xensource/license -jcf $(OUTPUT_FAKESERVER) .
	install -m 644 -o root -g root $(REPO)/ocaml/xe-cli/rt/geneva/testpatch.asc $(OUTPUT_PATCH)
	rm -rf $(OUTPUT_DATAMODEL_DIR)
	mkdir -p $(OUTPUT_DATAMODEL_DIR)
	install -m 644 -o root -g root $(REPO)/ocaml/idl/datamodel.a $(OUTPUT_DATAMODEL_DIR)
	install -m 644 -o root -g root $(REPO)/ocaml/idl/datamodel.cmxa $(OUTPUT_DATAMODEL_DIR)
	install -m 644 -o root -g root $(REPO)/ocaml/idl/datamodel.cmi $(OUTPUT_DATAMODEL_DIR)
	install -m 644 -o root -g root $(REPO)/ocaml/idl/datamodel_types.cmi $(OUTPUT_DATAMODEL_DIR)
	install -m 644 -o root -g root $(REPO)/ocaml/idl/datamodel_utils.cmi $(OUTPUT_DATAMODEL_DIR)
	install -m 644 -o root -g root $(REPO)/ocaml/idl/dm_api.cmi $(OUTPUT_DATAMODEL_DIR)
	install -m 644 -o root -g root $(REPO)/ocaml/idl/api_messages.cmi $(OUTPUT_DATAMODEL_DIR)

$(RPM_SOURCESDIR)/xe: $(OUTPUT_CLI)
	mkdir -p $(RPM_SOURCESDIR)
	cp $< $@

$(RPM_SOURCESDIR)/bash-completion: $(REPO)/ocaml/xe-cli/bash-completion
	mkdir -p $(RPM_SOURCESDIR)
	cp $< $@

$(RPM_SPECSDIR)/xe.spec: xe.spec.in
	mkdir -p $(RPM_SPECSDIR)
	$(call brand,xe.spec.in) > $@

$(MY_OBJ_DIR)/.rpmbuild.cli.stamp: $(RPM_DIRECTORIES) $(RPM_SPECSDIR)/xe.spec $(CLI_RPM_SOURCES)
	$(RPMBUILD) --target i386 -ba $(RPM_SPECSDIR)/xe.spec
	touch $@

.PHONY: clean
clean:
	rm -f $(OUTPUT_PKG) $(OUTPUT_FAKESERVER) $(OUTPUT_CLI) $(OUTPUT_WEBZIP) $(OUTPUT_SRC)
	$(MAKE) -C $(REPO) clean
