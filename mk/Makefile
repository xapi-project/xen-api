ifdef B_BASE
include $(B_BASE)/common.mk
include $(B_BASE)/rpmbuild.mk
REPO = $(call git_loc,xen-api-libs)
else
REPO ?= $(CURDIR)
%/.dirstamp:
	@mkdir -p $*
	@touch $@
endif
RPM_SPECSDIR?=$(shell rpm --eval='%_specdir')
RPM_SRPMSDIR?=$(shell rpm --eval='%_srcrpmdir')
RPM_SOURCESDIR?=$(shell rpm --eval='%_sourcedir')
RPM_RPMSDIR?=$(shell rpm --eval='%_rpmdir')
RPMBUILD?=rpmbuild

XAPI_LIBS_RELEASE=$(shell git rev-list HEAD | wc -l )
RPM_BINDIR=$(RPM_RPMSDIR)/$(DOMAIN0_ARCH_OPTIMIZED)
SPECNAME=xapi-libs.spec

.PHONY: build
build: $(MY_SOURCES)/MANIFEST rpm

.PHONY: rpm
rpm:
	mkdir -p $(RPM_RPMSDIR)
	$(MAKE) -C $(REPO) -f Makefile.xcp srpm
	mkdir -p $(RPM_RPMSDIR)/$(DOMAIN_ZERO_OPTIMIZED)
	$(RPMBUILD) --target $(DOMAIN0_ARCH_OPTIMIZED) -bb  $(RPM_SPECSDIR)/$(SPECNAME)
	mkdir -p $(MY_MAIN_PACKAGES)
	cp $(RPM_RPMSDIR)/$(DOMAIN0_ARCH_OPTIMIZED)/xapi-libs-fe-*.rpm $(MY_MAIN_PACKAGES)

$(MY_SOURCES)/MANIFEST: rpm $(MY_SOURCES_DIRSTAMP)
	rm -f $@
	/bin/sh ./srpms-to-manifest api-libs $(MY_OUTPUT_DIR)/SRPMS > $@

.PHONY: clean
	rm -f *.rpm
