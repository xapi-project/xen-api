PYTHON_PREFIX?=/usr
OPAM_PREFIX?=$(shell opam config var prefix)
OPAM_LIBDIR?=$(shell opam config var lib)

.PHONY: release build install uninstall clean test examples doc reindent

release:
	jbuilder build @install
	jbuilder build @python
	make -C _build/default/python

build:
	jbuilder build @install --dev
	jbuilder build @python --dev
	make -C _build/default/python

install:
	jbuilder install --prefix=$(OPAM_PREFIX) --libdir=$(OPAM_LIBDIR) xapi-storage
	make -C _build/default/python install PREFIX=$(PYTHON_PREFIX)

uninstall:
	jbuilder uninstall --prefix=$(OPAM_PREFIX) --libdir=$(OPAM_LIBDIR) xapi-storage
	make -C _build/default/python uninstall

clean:
	jbuilder clean

test:
	jbuilder runtest

examples:
	jbuilder build @gen_examples

# requires odoc
doc:
	jbuilder build @doc

reindent:
	git ls-files '*.ml*' | xargs ocp-indent --syntax cstruct -i


.DEFAULT_GOAL := release
