ifdef B_BASE
include $(B_BASE)/common.mk
include $(B_BASE)/rpmbuild.mk
REPO = $(call gitloc,xen-api-libs)
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
build: srpm $(MY_SOURCES)/MANIFEST
	mkdir -p $(RPM_RPMSDIR)
	$(shell  cd .. ; ./autogen.sh)
	$(RPMBUILD) --target $(DOMAIN0_ARCH_OPTIMIZED) -bb  $(RPM_SPECSDIR)/$(SPECNAME)
	mkdir -p $(MY_MAIN_PACKAGES)
	cp $(RPM_RPMSDIR)/$(DOMAIN0_ARCH_OPTIMIZED)/xapi-libs-fe-*.rpm $(MY_MAIN_PACKAGES)

.PHONY: srpm
srpm:
	mkdir -p $(RPM_SRPMSDIR) $(RPM_SPECSDIR) $(RPM_SOURCESDIR) $(RPM_RPMSDIR)
	while ! [ -d .git ]; do cd ..; done; \
	git archive --prefix=xapi-libs-0.1/ --format=tar HEAD | bzip2 -z > $(RPM_SOURCESDIR)/xapi-libs-0.1.tar.bz2  # xen-api-libs/mk/Makefile
	cp ../$(SPECNAME).in $(RPM_SPECSDIR)
	sed -e s\,@XAPI_LIBS_RELEASE@,$(XAPI_LIBS_RELEASE),g  \
            < $(RPM_SPECSDIR)/$(SPECNAME).in \
            > $(RPM_SPECSDIR)/$(SPECNAME)
	$(RPMBUILD) -bs $(RPM_SPECSDIR)/xapi-libs.spec

$(MY_SOURCES)/MANIFEST: $(MY_SOURCES_DIRSTAMP)
	rm -f $@
	/bin/sh ./srpms-to-manifest api-libs $(MY_OUTPUT_DIR)/SRPMS > $@

.PHONY: clean
	rm -f *.rpm
