.PHONY: all clean install build
all: build test doc

NAME=xapi-storage

setup.bin: setup.ml
	ocamlopt.opt -o $@ $< || ocamlopt -o $@ $< || ocamlc -o $@ $<
	rm -f setup.cmx setup.cmi setup.o setup.cmo

setup.data: setup.bin
	./setup.bin -configure --enable-tests

build: setup.data setup.bin
	./setup.bin -build

doc: setup.data setup.bin
	./setup.bin -doc

install: setup.bin
	./setup.bin -install

.PHONY: uninstall
uninstall:
	ocamlfind remove $(NAME) || true

test: setup.bin build
	./setup.bin -test

fulltest: setup.bin build
	./setup.bin -test

reinstall: setup.bin
	ocamlfind remove $(NAME) || true
	./setup.bin -reinstall

clean:
	ocamlbuild -clean
	rm -f setup.data setup.log setup.bin
