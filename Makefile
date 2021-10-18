include config.mk
DESTDIR?=`opam config var prefix 2>/dev/null`

.PHONY: release build install uninstall clean test doc format

release:
	dune build @install --profile=release
	dune build @python --profile=release

build:
	dune build @install
	dune build @python

install:
	dune install --destdir=$(DESTDIR) --prefix=$(PREFIX) --libdir=$(LIBDIR)
	make -C _build/default/xapi-storage/python
	make -C _build/default/xapi-storage/python install DESTDIR=$(DESTDIR)

uninstall:
	dune uninstall --destdir=$(DESTDIR) --prefix=$(PREFIX) --libdir=$(LIBDIR)
	make -C _build/default/python uninstall DESTDIR=$(DESTDIR)

clean:
	dune clean

test:
	dune runtest --no-buffer --profile=release

lint:
	dune build @python
	pylint --disable=line-too-long,too-few-public-methods,unused-argument,no-self-use,invalid-name,broad-except,protected-access,redefined-builtin,too-many-lines,wildcard-import,too-many-branches,too-many-arguments,unused-wildcard-import,raising-format-tuple,too-many-statements,duplicate-code _build/default/xapi-storage/python/xapi/storage/api/v5/*.py
	pycodestyle --ignore=E501 _build/default/xapi-storage/python/xapi/storage/api/v5/*.py


test-quick:
	dune build @runtest-quick  --profile=$(PROFILE)

# requires odoc
doc:
	dune build @doc --profile=release

format:
	dune build @fmt --auto-promote

.DEFAULT_GOAL := release
