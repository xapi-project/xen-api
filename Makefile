all: build

TESTS_FLAG=--enable-tests

NAME=rrdd-plugin
J=4

LIBDIR=_build/lib

setup.ml: _oasis
	oasis setup

setup.data: setup.ml
	ocaml setup.ml -configure $(TESTS_FLAG)

build: setup.data
	ocaml setup.ml -build -j $(J)

install: setup.data
	ocaml setup.ml -install

uninstall:
	ocamlfind remove $(NAME)

reinstall: setup.data
	ocamlfind remove $(NAME) || true
	ocaml setup.ml -reinstall

clean:
	ocamlbuild -clean
	rm -f setup.data setup.log
