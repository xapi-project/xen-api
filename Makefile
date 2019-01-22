PYTHON_PREFIX?=/usr
OPAM_PREFIX?=$(shell opam config var prefix)
OPAM_LIBDIR?=$(shell opam config var lib)
PROFILE=release

.PHONY: build install uninstall clean test lint doc reindent

build:
	dune build @install --profile=$(PROFILE)
	dune build @python --profile=$(PROFILE)
	make -C _build/default/python

install:
	dune install --prefix=$(OPAM_PREFIX) --libdir=$(OPAM_LIBDIR) xapi-storage
	make -C _build/default/python install PREFIX=$(PYTHON_PREFIX)

uninstall:
	dune uninstall --prefix=$(OPAM_PREFIX) --libdir=$(OPAM_LIBDIR) xapi-storage
	make -C _build/default/python uninstall

clean:
	dune clean

test:
	dune runtest

lint:
	dune build @python
	pylint --disable=line-too-long,too-few-public-methods,unused-argument,no-self-use,invalid-name,broad-except,protected-access,redefined-builtin,too-many-lines,wildcard-import,too-many-branches,too-many-arguments,unused-wildcard-import,raising-format-tuple,too-many-statements,duplicate-code _build/default/python/xapi/storage/api/v5/*.py
	pycodestyle --ignore=E501 _build/default/python/xapi/storage/api/v5/*.py

# requires odoc
doc:
	dune build @doc

reindent:
	git ls-files '*.ml*' | xargs ocp-indent --syntax cstruct -i
