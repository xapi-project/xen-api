OPAM_PREFIX=$(DESTDIR)$(shell opam config var prefix)
OPAM_LIBDIR=$(DESTDIR)$(shell opam config var lib)

.PHONY: build release install uninstall clean test doc reindent

release:
	    jbuilder build @install -j $$(getconf _NPROCESSORS_ONLN)

build:
	    jbuilder build @install --dev -j $$(getconf _NPROCESSORS_ONLN)


install:
	    jbuilder install --prefix=$(OPAM_PREFIX) --libdir=$(OPAM_LIBDIR)

uninstall:
	    jbuilder uninstall

clean:
	    jbuilder clean

test:
	    jbuilder runtest --dev -j $$(getconf _NPROCESSORS_ONLN)

# requires odoc
doc:
	    jbuilder build @doc

gh-pages:
	    bash .docgen.sh

reindent:
	    git ls-files '*.ml' '*.mli' | xargs ocp-indent --syntax cstruct -i

runtime-coverage:
	    BISECT_RUNTIME=YES make
