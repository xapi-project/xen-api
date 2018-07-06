.PHONY: all clean fuzz build-fuzz

all:
	jbuilder build @install --dev

clean:
	jbuilder clean

test:
	jbuilder runtest --dev

build-fuzz:
	jbuilder build --dev fuzz/fuzz.exe

fuzz: build-fuzz
	mkdir -p _build/in
	echo > _build/in/empty
	afl-fuzz -i _build/in -o _build/out -- _build/default/fuzz/fuzz.exe @@

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
