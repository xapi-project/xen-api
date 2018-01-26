OPAM_PREFIX?=$(DESTDIR)$(shell opam config var prefix)
OPAM_LIBDIR?=$(DESTDIR)$(shell opam config var lib)

URL ?= https://github.com/xapi-project/xen-api
BRANCH ?= master

.PHONY: release build async-examples lwt-examples install uninstall clean test doc reindent regenerate

release:
	jbuilder build @install

build:
	jbuilder build @install --dev

async-examples:
	jbuilder build @async_examples/examples

lwt-examples:
	jbuilder build @lwt_examples/examples

install:
	jbuilder install --prefix=$(OPAM_PREFIX) --libdir=$(OPAM_LIBDIR)

uninstall:
	jbuilder uninstall --prefix=$(OPAM_PREFIX) --libdir=$(OPAM_LIBDIR)

clean:
	jbuilder clean

test:
	jbuilder runtest

# requires odoc
doc:
	jbuilder build @doc

gh-pages:
	bash .docgen.sh

reindent:
	git ls-files '*.ml' '*.mli' | xargs ocp-indent --syntax cstruct -i

