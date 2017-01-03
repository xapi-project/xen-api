# makefile for api component in build system
USE_BRANDING := yes
USE_BUILD_NUMBER := yes
IMPORT_BRANDING := yes
include $(B_BASE)/common.mk
include $(B_BASE)/rpmbuild.mk

REPO                   = $(call git_loc,xen-api)

OUTPUT_XAPI_SRC        = $(MY_SOURCES)/xen-api-src.tar.bz2

OUTPUT_CLI_RPM_SOURCES = $(RPM_SOURCESDIR)/xe $(RPM_SOURCESDIR)/bash-completion

OUTPUT_DOCS            = $(MY_OUTPUT_DIR)/sdkiso
OUTPUT_WEBZIP          = $(MY_OUTPUT_DIR)/sdk-html.zip

JQUERY_PACK_DIST       = $(CARBON_DISTFILES)/javascript/jquery/jquery-1.1.3.1.pack.js
JQUERY_TV_DIST         = $(CARBON_DISTFILES)/javascript/jquery/treeview/jquery.treeview.zip

OUTPUT_DATAMODEL_DIR   = $(MY_OUTPUT_DIR)/datamodel

OUTPUT_SDK_DIR         = $(MY_OUTPUT_DIR)
OUTPUT_SDK             = $(MY_OUTPUT_DIR)/sdk.tar.gz

export BUILD_NUMBER
export PLATFORM_NAME
export PLATFORM_VERSION
export PRODUCT_BRAND
export PRODUCT_VERSION
export PRODUCT_VERSION_TEXT
export PRODUCT_VERSION_TEXT_SHORT

.PHONY: build
build: $(OUTPUT_CLI_RT) $(OUTPUT_SDK) $(MY_SOURCES)/MANIFEST
	$(call mkdir_clean,$(MY_MAIN_CDFILES)/client_install)
	install -m 755 $(RPM_RPMSDIR)/$(DOMAIN0_ARCH_OPTIMIZED)/xapi-xe-*.rpm $(MY_MAIN_CDFILES)/client_install/xe-cli-$(PRODUCT_VERSION)-$(BUILD_NUMBER).$(DOMAIN0_ARCH_OPTIMIZED).rpm
	# Delete this as soon as xenrt has been updated to fetch the file from
	# the new place:
	$(call mkdir_clean,$(MY_LINUX_CDFILES)/client_install)
	install -m 755 $(RPM_RPMSDIR)/$(DOMAIN0_ARCH_OPTIMIZED)/xapi-xe-*.rpm $(MY_LINUX_CDFILES)/client_install/xe-cli-$(PRODUCT_VERSION)-$(BUILD_NUMBER).$(DOMAIN0_ARCH_OPTIMIZED).rpm

$(MY_SOURCES)/MANIFEST: $(MY_SOURCES_DIRSTAMP) $(OUTPUT_XAPI_SRC)
	rm -f $@
	/bin/sh ./srpms-to-manifest api $(MY_OUTPUT_DIR)/SRPMS > $@
	echo api gpl file $(JQUERY_PACK_DIST) >> $@
	echo api gpl file $(JQUERY_TV_DIST) >> $@

$(OUTPUT_XAPI_SRC):
	if cd $(REPO); then \
		while ! [ -d .git ]; do cd ..; done; \
		git archive --prefix=$$(echo $@ | sed -r "s/([^/]*\/)*([^./]*)(.[^./]*)*/\2\//") --format=tar HEAD | bzip2 -z > $@; \
	fi; # git archive in xen-api/mk/Makefile

$(OUTPUT_SDK): $(MY_MAIN_PACKAGES)/.dirstamp $(RPM_DIRECTORIES)
	(cd $(REPO); ./configure)
	$(MAKE) clean
	$(MAKE) -C $(REPO) version
	$(MAKE) -C $(REPO) srpm
	mkdir -p $(RPM_RPMSDIR)/$(DOMAIN_ZERO_OPTIMIZED)
	$(RPMBUILD) --rebuild --target $(DOMAIN0_ARCH_OPTIMIZED) $(MY_OUTPUT_DIR)/SRPMS/xapi-*.src.rpm

	# extract the xapi-docs
	mkdir -p $(OUTPUT_SDK_DIR)/tmp
	(cd $(OUTPUT_SDK_DIR)/tmp; rpm2cpio $(RPM_RPMSDIR)/*/xapi-docs*.rpm | cpio -i --make-directories; tar -zcf $(OUTPUT_SDK) .;zip -9rv $(OUTPUT_WEBZIP) .)
	rm -rf $(OUTPUT_SDK_DIR)/tmp

	# Make sure the right RPMs appear on the install CD
	mkdir -p $(MY_MAIN_PACKAGES)
	cp $(RPM_RPMSDIR)/$(DOMAIN0_ARCH_OPTIMIZED)/xapi-core-*.rpm $(MY_MAIN_PACKAGES)
	cp $(RPM_RPMSDIR)/$(DOMAIN0_ARCH_OPTIMIZED)/xapi-tests-*.rpm $(MY_MAIN_PACKAGES)
	cp $(RPM_RPMSDIR)/*/xapi-www-*.rpm $(MY_MAIN_PACKAGES)
	cp $(RPM_RPMSDIR)/$(DOMAIN0_ARCH_OPTIMIZED)/xapi-xe-*.rpm $(MY_MAIN_PACKAGES)

.PHONY: clean
clean:
	rm -f $(OUTPUT_XAPI_SRC) $(OUTPUT_WEBZIP) $(OUTPUT_SDK)
	rm -rf $(OUTPUT_DATAMODEL_DIR) $(OUTPUT_DOCS) $(OUTPUT_SDK_DIR)/tmp \
	  $(MY_OUTPUT_DIR)/SRPMS $(RPM_RPMSDIR) $(MY_MAIN_PACKAGES) $(MY_XCP_PACKAGES)
	$(MAKE) -C $(REPO) clean
