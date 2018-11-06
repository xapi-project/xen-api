.PHONY: build release lint test install uninstall clean reindent

release:
	dune build -p vhd-tool -j $$(getconf _NPROCESSORS_ONLN)

build:
	dune build @install

lint:
	pycodestyle scripts/*.py
	pylint --disable too-many-locals scripts/get_nbd_extents.py
	pylint --disable fixme,too-many-arguments,too-many-instance-attributes scripts/python_nbd_client.py

test: lint
	dune runtest

stresstest:
	dune build @stresstest

install:
	dune install -p vhd-tool

uninstall:
	dune uninstall -p vhd-tool

clean:
	dune clean

reindent:
	ocp-indent --inplace **/*.ml*
