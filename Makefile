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
	$(MAKE) -C log
	$(MAKE) -C stunnel
	$(MAKE) -C pciutil
	$(MAKE) -C cdrom
	$(MAKE) -C xml-light2
	$(MAKE) -C rss
	$(MAKE) -C rpc-light
	$(MAKE) -C http-svr
	$(MAKE) -C close-and-exec
	$(MAKE) -C sexpr

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
	$(MAKE) -C log install
	$(MAKE) -C stunnel install
	$(MAKE) -C pciutil install
	$(MAKE) -C cdrom install
	$(MAKE) -C xml-light2 install
	$(MAKE) -C rss install
	$(MAKE) -C rpc-light install
	$(MAKE) -C http-svr install
	$(MAKE) -C close-and-exec install
	$(MAKE) -C sexpr install

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
	$(MAKE) -C log uninstall
	$(MAKE) -C stunnel uninstall
	$(MAKE) -C pciutil uninstall
	$(MAKE) -C cdrom uninstall
	$(MAKE) -C xml-light2 uninstall
	$(MAKE) -C rss uninstall
	$(MAKE) -C rpc-light uninstall
	$(MAKE) -C http-svr uninstall
	$(MAKE) -C close-and-exec uninstall
	$(MAKE) -C sexpr uninstall

uninstallxen:
	$(MAKE) -C eventchn uninstall
	$(MAKE) -C xsrpc uninstall
	$(MAKE) -C xs uninstall
	$(MAKE) -C xb uninstall
	$(MAKE) -C xc uninstall
	$(MAKE) -C mmap uninstall

bins:
	$(MAKE) -C pciutil bins
	$(MAKE) -C xml-light2 bins
	$(MAKE) -C sexpr bins
	$(MAKE) -C stdext bins
	$(MAKE) -C close-and-exec bins

bininstall:
	$(MAKE) -C pciutil bininstall
	$(MAKE) -C xml-light2 bininstall
	$(MAKE) -C sexpr bininstall
	$(MAKE) -C stdext bininstall
	$(MAKE) -C close-and-exec bininstall

binuninstall:
	$(MAKE) -C pciutil binuninstall
	$(MAKE) -C xml-light2 binuninstall
	$(MAKE) -C sexpr binuninstall
	$(MAKE) -C stdext binuninstall
	$(MAKE) -C close-and-exec binuninstall

OUTPUT_API_PKG := $(MY_OUTPUT_DIR)/api-libs.tar.gz

$(OUTPUT_API_PKG): DESTDIR=$(MY_OBJ_DIR)/staging/
$(OUTPUT_API_PKG): PREFIX=$(shell ocamlfind printconf path)
$(OUTPUT_API_PKG): $(MY_OBJ_DIR)/.dirstamp $(MY_OUTPUT_DIR)/.dirstamp
	rm -rf $(DESTDIR)
	mkdir -p $(DESTDIR)$(PREFIX)
	mkdir -p $(DESTDIR)$(LIBEXEC)
	$(MAKE) clean
	$(MAKE) all
	$(MAKE) DESTDIR=$(MY_OBJ_DIR)/staging install
	$(MAKE) bins
	$(MAKE) DESTDIR=$(MY_OBJ_DIR)/staging bininstall
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
	make -C log clean
	make -C stunnel clean
	make -C pciutil clean
	make -C cdrom clean
	make -C xml-light2 clean
	make -C rss clean
	make -C rpc-light clean
	make -C http-svr clean
	make -C close-and-exec clean
	make -C sexpr clean
	rm -f $(OUTPUT_API_PKG)

cleanxen:
	$(MAKE) -C mmap clean
	$(MAKE) -C xc clean
	$(MAKE) -C xb clean
	$(MAKE) -C xs clean
	$(MAKE) -C xsrpc clean
	$(MAKE) -C eventchn clean
	rm -f $(OUTPUT_XAPI_PKG)
