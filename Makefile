PYTHON_PREFIX?=/usr
OPAM_PREFIX?=$(shell opam config var prefix)
OPAM_LIBDIR?=$(shell opam config var lib)

.PHONY: release build install uninstall clean test lint doc reindent

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

lint:
	jbuilder build @python
	pylint --disable=line-too-long,too-few-public-methods,unused-argument,no-self-use,invalid-name,broad-except,protected-access,redefined-builtin,too-many-lines,wildcard-import,too-many-branches,too-many-arguments,unused-wildcard-import,raising-format-tuple,too-many-statements,duplicate-code _build/default/python/xapi/storage/api/v4/*.py
	pycodestyle --ignore=E501 _build/default/python/xapi/storage/api/v4/*.py

# requires odoc
doc:
	jbuilder build @doc

reindent:
	git ls-files '*.ml*' | xargs ocp-indent --syntax cstruct -i


.DEFAULT_GOAL := release
