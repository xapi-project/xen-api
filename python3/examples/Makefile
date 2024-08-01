PROFILE=release
SETUPTOOLS_SCM_PRETEND_VERSION ?= ""

.PHONY: build clean

build:
	SETUPTOOLS_SCM_PRETEND_VERSION=$(XAPI_VERSION) python -m build --wheel .
	SETUPTOOLS_SCM_PRETEND_VERSION=$(XAPI_VERSION) python -m build --sdist .

clean:
	dune clean
	rm -rf dist/ build/ XenAPI.egg-info/
