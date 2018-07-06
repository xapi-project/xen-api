JBUILDER ?= jbuilder

all:
	@$(JBUILDER) build @install @DEFAULT

test:
	@$(JBUILDER) runtest

check: test

clean:
	@$(JBUILDER) clean

.PHONY: check test all clean

REPO=../opam-repository
PACKAGES=$(REPO)/packages

pkg-%:
	topkg opam pkg -n $*
	mkdir -p $(PACKAGES)/$*
	cp -r _build/$*.* $(PACKAGES)/$*/
	rm -f $(PACKAGES)/$*/$*.opam
	cd $(PACKAGES) && git add $*

PKGS=$(basename $(wildcard *.opam))
opam-pkg:
	$(MAKE) $(PKGS:%=pkg-%)
