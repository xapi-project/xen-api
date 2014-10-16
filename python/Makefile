PREFIX?=/

.PHONY: build
build:
	python setup.py build

.PHONY: clean
clean:
	python setup.py clean
	rm -rf build

.PHONY: install
install: build
	python setup.py install --prefix $(PREFIX)
