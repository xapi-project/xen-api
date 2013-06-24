include $(B_BASE)/common.mk
include $(B_BASE)/rpmbuild.mk

TARGETS = rrdp_iostat \
	 rrdp_squeezed \
	 rrdp_xenpm

.PHONY: build
build: $(TARGETS)

$(TARGETS):
	omake $@

.PHONY: clean
clean:
	omake clean

.PHONY: install
install: $(TARGETS)
	omake install
