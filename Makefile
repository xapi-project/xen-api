NO_DEFAULT_BUILD := yes
ifdef B_BASE
include $(B_BASE)/common.mk
else
MY_OUTPUT_DIR ?= $(CURDIR)/output
MY_OBJ_DIR ?= $(CURDIR)/obj

%/.dirstamp:
	@mkdir -p $*
	@touch $@
endif

all:
	$(MAKE) -C uuid
	$(MAKE) -C camldm
	$(MAKE) -C stdext
	$(MAKE) -C cdrom
	$(MAKE) -C log
	$(MAKE) -C sha1
	$(MAKE) -C xml-light2
	$(MAKE) -C rpc-light
 
allxen:
	$(MAKE) -C mmap
	$(MAKE) -C xc
	$(MAKE) -C xb
	$(MAKE) -C xs
	$(MAKE) -C xsrpc
	$(MAKE) -C eventchn

install:
	$(MAKE) -C uuid install
	$(MAKE) -C camldm install
	$(MAKE) -C stdext install
	$(MAKE) -C cdrom install
	$(MAKE) -C log install
	$(MAKE) -C sha1 install
	$(MAKE) -C xml-light2 install
	$(MAKE) -C rpc-light install

installxen:
	$(MAKE) -C mmap install
	$(MAKE) -C xc install
	$(MAKE) -C xb install
	$(MAKE) -C xs install
	$(MAKE) -C xsrpc install
	$(MAKE) -C eventchn install

uninstall:
	$(MAKE) -C uuid uninstall
	$(MAKE) -C camldm uninstall
	$(MAKE) -C stdext uninstall
	$(MAKE) -C cdrom uninstall
	$(MAKE) -C log uninstall
	$(MAKE) -C sha1 uninstall
	$(MAKE) -C xml-light2 uninstall
	$(MAKE) -C rpc-light uninstall

uninstallxen:
	$(MAKE) -C eventchn uninstall
	$(MAKE) -C xsrpc uninstall
	$(MAKE) -C xs uninstall
	$(MAKE) -C xb uninstall
	$(MAKE) -C xc uninstall
	$(MAKE) -C mmap uninstall

OUTPUT_API_PKG := $(MY_OUTPUT_DIR)/api-libs.tar.gz

$(OUTPUT_API_PKG): DESTDIR=$(MY_OBJ_DIR)/staging/
$(OUTPUT_API_PKG): PREFIX=$(shell ocamlfind printconf path)
$(OUTPUT_API_PKG): $(MY_OBJ_DIR)/.dirstamp $(MY_OUTPUT_DIR)/.dirstamp
	rm -rf $(DESTDIR)
	mkdir -p $(DESTDIR)$(PREFIX)
	$(MAKE) clean
	$(MAKE) all
	$(MAKE) DESTDIR=$(MY_OBJ_DIR)/staging install
	tar -C $(DESTDIR) -zcf $@ .

OUTPUT_XAPI_PKG := $(MY_OUTPUT_DIR)/xapi-libs.tar.gz

$(OUTPUT_XAPI_PKG): DESTDIR=$(MY_OBJ_DIR)/staging/
$(OUTPUT_XAPI_PKG): PREFIX=$(shell ocamlfind printconf path)
$(OUTPUT_XAPI_PKG): $(MY_OBJ_DIR)/.dirstamp $(MY_OUTPUT_DIR)/.dirstamp
	rm -rf $(DESTDIR)
	mkdir -p $(DESTDIR)$(PREFIX)
	$(MAKE) cleanxen
	$(MAKE) allxen
	$(MAKE) DESTDIR=$(MY_OBJ_DIR)/staging installxen
	tar -C $(DESTDIR) -zcf $@ .

OUTPUT_SRC := $(MY_OUTPUT_DIR)/xen-api-libs-src.tar.bz2

$(MY_SOURCES)/MANIFEST: $(MY_SOURCES_DIRSTAMP) $(OUTPUT_SRC)
	echo api lgpl-with-linking-exception file $(OUTPUT_SRC) > $@

$(OUTPUT_SRC):
	cd $(REPO) && hg archive -t tbz2 $(HG_EXCLUDE) $@

.PHONY: api-libs
api-libs: $(OUTPUT_API_PKG) $(MY_SOURCES)/MANIFEST
	@ :

.PHONY: xapi-libs
xapi-libs: $(OUTPUT_XAPI_PKG) $(MY_SOURCES)/MANIFEST
	@ :

.PHONY: clean
clean:
	make -C uuid clean
	make -C camldm clean
	make -C stdext clean
	make -C cdrom clean
	make -C log clean
	make -C sha1 clean
	make -C xml-light2 clean
	make -C rpc-light clean
	rm -f $(OUTPUT_API_PKG)

cleanxen:
	$(MAKE) -C mmap clean
	$(MAKE) -C xc clean
	$(MAKE) -C xb clean
	$(MAKE) -C xs clean
	$(MAKE) -C xsrpc clean
	$(MAKE) -C eventchn clean
	rm -f $(OUTPUT_XAPI_PKG)
