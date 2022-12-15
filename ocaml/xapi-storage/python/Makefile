PREFIX?=/usr
PYTHON?=python2

.PHONY: build release clean install uninstall

build:
	$(PYTHON) setup.py build

release: build

clean:
	$(PYTHON) setup.py clean
	rm -rf build

install: build
	$(PYTHON) setup.py install --prefix $(PREFIX) --root $(DESTDIR)

uninstall:
	@ echo "I don't know how to uninstall python code"

.DEFAULT_GOAL := release
