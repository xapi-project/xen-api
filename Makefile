
default: build

.PHONY: build
build:
	(cd generator; make)
	mkdir -p ocaml/examples
	./generator/main.native
	make -C ocaml

.PHONY: install
install:
	make -C ocaml install

.PHONY: reinstall
reinstall:
	make -C ocaml reinstall

.PHONY: clean
clean:
	make -C generator clean
	make -C ocaml clean
