NO_DEFAULT_BUILD := yes
ifdef B_BASE
include $(B_BASE)/common.mk
else
MY_OUTPUT_DIR         ?= $(CURDIR)/_build/output-devel
MY_MAIN_PACKAGES      ?= $(CURDIR)/_build/output
MY_OBJ_DIR            ?= $(CURDIR)/_build/obj
MY_SOURCES            ?= $(CURDIR)/_build/output-src
MY_SOURCES_DIRSTAMP   ?= $(MY_SOURCES)/.dirstamp

%/.dirstamp:
	@mkdir -p $*
	@touch $@
endif

REPO                   = $(call hg_loc,xen-api-libs)
STAGING               ?= $(MY_OBJ_DIR)/staging
OUTPUT_API_DEVEL_PKG  ?= $(MY_OUTPUT_DIR)/api-libs-devel.tar.gz
OUTPUT_XAPI_DEVEL_PKG ?= $(MY_OUTPUT_DIR)/xapi-libs-devel.tar.gz
OUTPUT_API_PKG        ?= $(MY_MAIN_PACKAGES)/api-libs.tar.bz2
OUTPUT_SRC            ?= $(MY_SOURCES)/xen-api-libs-src.tar.bz2

$(OUTPUT_API_DEVEL_PKG):
	rm -rf $(STAGING)
	mkdir -p $(MY_OUTPUT_DIR)
	(cd $(REPO) && sh autogen.sh && ./configure)
	$(MAKE) -C $(REPO) clean
	$(MAKE) -C $(REPO) all
	$(MAKE) -C $(REPO) DESTDIR=$(STAGING) install
	tar -C $(STAGING) -zcf $@ .

$(OUTPUT_API_PKG): $(OUTPUT_API_DEVEL_PKG)
	rm -rf $(STAGING)
	mkdir -p $(MY_MAIN_PACKAGES)
	(cd $(REPO) && sh autogen.sh && ./configure)
	$(MAKE) -C $(REPO) bins
	$(MAKE) -C $(REPO) DESTDIR=$(STAGING) bininstall
	tar -C $(STAGING) -jcf $@ .

$(OUTPUT_XAPI_DEVEL_PKG):
	rm -rf $(STAGING)
	mkdir -p $(MY_OUTPUT_DIR)
	(cd $(REPO) && sh autogen.sh && ./configure)
	$(MAKE) -C $(REPO) cleanxen
	$(MAKE) -C $(REPO) allxen
	$(MAKE) -C $(REPO) DESTDIR=$(STAGING) installxen
	tar -C $(STAGING) -zcf $@ .

$(MY_SOURCES)/MANIFEST: $(MY_SOURCES_DIRSTAMP) $(OUTPUT_SRC)
	mkdir -p $(MY_SOURCES)
	echo api lgpl-with-linking-exception file $(OUTPUT_SRC) > $@

$(OUTPUT_SRC):
	mkdir -p $(MY_SOURCES)
	cd $(REPO) && hg archive -t tbz2 $(HG_EXCLUDE) $@

.PHONY: api-libs
api-libs: $(OUTPUT_API_DEVEL_PKG) $(OUTPUT_API_PKG) $(MY_SOURCES)/MANIFEST

.PHONY: xapi-libs
xapi-libs: $(OUTPUT_XAPI_DEVEL_PKG) $(MY_SOURCES)/MANIFEST

.PHONY: clean
	rm -f $(STAGING)
	rm -f $(OUTPUT_API_DEVEL_PKG)
	rm -f $(OUTPUT_API_PKG)
	rm -f $(OUTPUT_XAPI_DEVEL_PKG)
	rm -f $(OUTPUT_SRC)
	$(MAKE) -C $(REPO) clean
	$(MAKE) -C $(REPO) cleanxen
