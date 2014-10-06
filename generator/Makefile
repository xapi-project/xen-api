.PHONY: all clean install build reinstall uninstall distclean
all: build

clean:
	@rm -f setup.data setup.log setup.bin
	@rm -rf _build
	@rm -f *.native

setup.bin: setup.ml
	@ocamlopt.opt -o $@ $< || ocamlopt -o $@ $< || ocamlc -o $@ $<
	@rm -f setup.cmx setup.cmi setup.o setup.cmo

setup.data: setup.bin
	@./setup.bin -configure

build: setup.data setup.bin
	@./setup.bin -build

install:
	echo hello

reinstall: install

uninstall:
	echo hello
