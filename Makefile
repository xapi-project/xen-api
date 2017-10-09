.PHONY: build release install uninstall clean test doc reindent

build:
	    jbuilder build @install --dev -j $$(getconf _NPROCESSORS_ONLN)

release:
	    jbuilder build @install

install:
	    jbuilder install

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
