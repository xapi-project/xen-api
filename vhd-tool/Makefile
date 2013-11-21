.PHONY: build
build: dist/setup
	obuild build

dist/setup: vhd-tool.obuild
	obuild configure

.PHONY: clean
clean:
	rm -rf dist
