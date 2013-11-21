.PHONY: build
build: dist/setup
	obuild build

dist/setup: sparse_dd.obuild
	obuild configure

.PHONY: clean
clean:
	rm -rf dist
