
.PHONY: build clean test

build:
	jbuilder build @install --dev

test:
	jbuilder runtest --dev

install:
	jbuilder install

uninstall:
	jbuilder uninstall

.PHONY: docker
docker:
	docker build -t xen-gnt .

clean:
	rm -rf _build *.install

REPO=../../mirage/opam-repository
PACKAGES=$(REPO)/packages
# until we have https://github.com/ocaml/opam-publish/issues/38
pkg-%:
	topkg opam pkg -n $*
	mkdir -p $(PACKAGES)/$*
	cp -r _build/$*.* $(PACKAGES)/$*/
	cd $(PACKAGES) && git add $*

PKGS=$(basename $(wildcard *.opam))
opam-pkg:
	$(MAKE) $(PKGS:%=pkg-%)
