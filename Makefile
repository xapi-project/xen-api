
default: build

.PHONY: build
build:
	(cd generator; make)
	mkdir -p ocaml/examples
	./generator/main.native
	make -C ocaml
	make -C python

.PHONY: html
html: build
	./generator/main.native -html

.PHONY: install
install:
	make -C ocaml install
	make -C python install

.PHONY: reinstall
reinstall:
	make -C ocaml reinstall
	make -C python reinstall

.PHONY: clean
clean:
	make -C generator clean
	make -C ocaml clean
	make -C python clean
