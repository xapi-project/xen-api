PREFIX?=/usr

.PHONY: build release clean install uninstall

build:
	python setup.py build

release: build

clean:
	python setup.py clean
	rm -rf build

install: build
	python setup.py install --prefix $(PREFIX)

uninstall:
	@ echo "I don't know how to uninstall python code"

.DEFAULT_GOAL := release
