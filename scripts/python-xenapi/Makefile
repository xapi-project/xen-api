PYTHON := $(shell which python)

all:
	$(PYTHON) setup.py sdist

install:
	$(PYTHON) setup.py install

rpm:
	$(PYTHON) setup.py bdist --format=rpm

clean:
	rm -rf dist
	rm -rf build
	rm -rf MANIFEST
	rm -f *.pyc
