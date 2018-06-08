.PHONY: build release lint test install uninstall clean reindent

build:
	jbuilder build @install

release:
	jbuilder build @install

lint:
	pycodestyle scripts/*.py
	pylint --disable too-many-locals scripts/get_nbd_extents.py
	pylint --disable fixme,too-many-arguments,too-many-instance-attributes scripts/python_nbd_client.py

test: lint
	jbuilder runtest

stresstest:
	jbuilder build @stresstest

install:
	jbuilder install

uninstall:
	jbuilder uninstall

clean:
	jbuilder clean

reindent:
	ocp-indent --inplace **/*.ml*
