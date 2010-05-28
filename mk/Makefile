# makefile for api component in build system
USE_BRANDING := yes
USE_BUILD_NUMBER := yes
IMPORT_BRANDING := yes
include $(B_BASE)/common.mk
include $(B_BASE)/rpmbuild.mk

REPO                   = $(call hg_loc,xen-api)

STAGING                = $(REPO)/dist/staging
OUTPUT_XAPI            = $(MY_MAIN_PACKAGES)/xapi.tar.bz2
OUTPUT_XAPI_DEVEL      = $(MY_OUTPUT_DIR)/xapi-devel.tar.gz
OUTPUT_XAPI_SRC        = $(MY_SOURCES)/xen-api-src.tar.bz2

STAGING_CLI_RT         = $(REPO)/dist/cli-rt
OUTPUT_CLI_RT          = $(MY_OUTPUT_DIR)/cli-regress.tar.gz
OUTPUT_CLI_RPM_SOURCES = $(RPM_SOURCESDIR)/xe $(RPM_SOURCESDIR)/bash-completion

OUTPUT_DOCS            = $(MY_OUTPUT_DIR)/sdkiso
OUTPUT_WEBZIP          = $(MY_OUTPUT_DIR)/sdk-html.zip

JQUERY_PACK_DIST       = $(CARBON_DISTFILES)/javascript/jquery/jquery-1.1.3.1.pack.js
JQUERY_TV_DIST         = $(CARBON_DISTFILES)/javascript/jquery/treeview/jquery.treeview.zip

OUTPUT_DATAMODEL_DIR   = $(MY_OUTPUT_DIR)/datamodel

OUTPUT_SDK_DIR         = $(MY_OUTPUT_DIR)
OUTPUT_SDK             = $(MY_OUTPUT_DIR)/sdk.tar.gz

export BUILD_NUMBER
export PRODUCT_BRAND
export PRODUCT_VERSION

.PHONY: build
build: $(OUTPUT_XAPI) $(OUTPUT_XAPI_DEVEL) $(OUTPUT_CLI_RT) $(OUTPUT_SDK) $(MY_OBJ_DIR)/.rpmbuild.cli.stamp $(MY_SOURCES)/MANIFEST
	$(call mkdir_clean,$(MY_LINUX_CDFILES)/client_install)
	install -m 755 -o root -g root $(RPM_RPMSDIR)/i386/xe-cli-*.rpm $(MY_LINUX_CDFILES)/client_install/

$(MY_SOURCES)/MANIFEST: $(MY_SOURCES_DIRSTAMP) $(OUTPUT_XAPI_SRC)
	echo api gpl+lgpl-with-linking-exception file $(OUTPUT_XAPI_SRC) > $@
	echo api gpl file $(JQUERY_PACK_DIST) >> $@
	echo api gpl file $(JQUERY_TV_DIST) >> $@

$(OUTPUT_XAPI_SRC):
	cd $(REPO) && hg archive -t tbz2 $(HG_EXCLUDE) $@

$(OUTPUT_XAPI) $(OUTPUT_XAPI_DEVEL) $(OUTPUT_CLI_RT) $(OUTPUT_SDK): $(call hg_req,xen-api) $(MY_MAIN_PACKAGES)/.dirstamp
	$(MAKE) clean
	$(MAKE) -C $(REPO) version
	$(MAKE) -C $(REPO)

	rm -rf $(STAGING) $(STAGING_CLI_RT)
	$(MAKE) -C $(REPO) install
	mkdir -p $(MY_MAIN_PACKAGES)
	tar -C $(STAGING) -jcf $(OUTPUT_XAPI) .
	mkdir -p $(MY_OUTPUT_DIR)
	tar -C $(STAGING_CLI_RT) -zcf $(OUTPUT_CLI_RT) .

	rm -rf $(STAGING)
	$(MAKE) -C $(REPO) DESTDIR=$(STAGING) lib-install
	tar -C $(STAGING) -zcf $(OUTPUT_XAPI_DEVEL) .

	rm -rf $(STAGING)
	$(MAKE) -C $(REPO) sdk-install
	$(call mkdir_clean,$(OUTPUT_DOCS))
	cp $(STAGING)/SDK/README.html $(OUTPUT_DOCS)/
	cp -a $(STAGING)/SDK/docs $(OUTPUT_DOCS)/
	cd $(STAGING)/SDKwww && zip -9rv $(OUTPUT_WEBZIP) .
	mkdir -p $(OUTPUT_SDK_DIR)
	tar -C $(STAGING) -zcf $(OUTPUT_SDK) .

	rm -rf $(OUTPUT_DATAMODEL_DIR)
	mkdir -p $(OUTPUT_DATAMODEL_DIR)
	install -m 644 -o root -g root $(REPO)/ocaml/idl/datamodel.a $(OUTPUT_DATAMODEL_DIR)
	install -m 644 -o root -g root $(REPO)/ocaml/idl/datamodel.cmxa $(OUTPUT_DATAMODEL_DIR)
	install -m 644 -o root -g root $(REPO)/ocaml/idl/datamodel.cmi $(OUTPUT_DATAMODEL_DIR)
	install -m 644 -o root -g root $(REPO)/ocaml/idl/datamodel_types.cmi $(OUTPUT_DATAMODEL_DIR)
	install -m 644 -o root -g root $(REPO)/ocaml/idl/datamodel_utils.cmi $(OUTPUT_DATAMODEL_DIR)
	install -m 644 -o root -g root $(REPO)/ocaml/idl/dm_api.cmi $(OUTPUT_DATAMODEL_DIR)
	install -m 644 -o root -g root $(REPO)/ocaml/idl/api_messages.cmi $(OUTPUT_DATAMODEL_DIR)

$(RPM_SOURCESDIR)/xe: $(REPO)/ocaml/xe-cli/xe
	mkdir -p $(RPM_SOURCESDIR)
	cp $< $@

$(RPM_SOURCESDIR)/bash-completion: $(REPO)/ocaml/xe-cli/bash-completion
	mkdir -p $(RPM_SOURCESDIR)
	cp $< $@

$(RPM_SPECSDIR)/xe.spec: xe.spec.in
	mkdir -p $(RPM_SPECSDIR)
	$(call brand,xe.spec.in) > $@

$(MY_OBJ_DIR)/.rpmbuild.cli.stamp: $(RPM_DIRECTORIES) $(RPM_SPECSDIR)/xe.spec $(OUTPUT_CLI_RPM_SOURCES)
	$(RPMBUILD) --target i386 -ba $(RPM_SPECSDIR)/xe.spec
	touch $@

.PHONY: clean
clean:
	rm -f $(OUTPUT_XAPI) $(OUTPUT_XAPI_DEVEL) $(OUTPUT_XAPI_SRC) $(OUTPUT_CLI_RT) $(OUTPUT_WEBZIP) $(OUTPUT_SDK)
	rm -rf $(OUTPUT_DATAMODEL_DIR) $(OUTPUT_DOCS) $(OUTPUT_SDK_DIR)
	$(MAKE) -C $(REPO) clean
