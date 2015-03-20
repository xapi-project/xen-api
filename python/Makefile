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

.PHONY: uninstall
uninstall:
	echo "I don't know how to uninstall python code"
