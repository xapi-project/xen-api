all: build

TESTS_FLAG=--enable-tests

NAME=rrd-protocol
J=4

LIBDIR=_build/lib

setup.ml: _oasis
	oasis setup

setup.data: setup.ml
	ocaml setup.ml -configure $(TESTS_FLAG)

build: setup.data setup.ml
	ocaml setup.ml -build -j $(J)

doc: setup.data setup.ml
	ocaml setup.ml -doc -j $(J)

install: setup.data setup.ml
	ocaml setup.ml -install

uninstall:
	ocamlfind remove $(NAME)

reinstall: setup.ml
	ocamlfind remove $(NAME) || true
	ocaml setup.ml -reinstall

clean:
	ocamlbuild -clean
	rm -f setup.data setup.log
