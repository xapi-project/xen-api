.PHONY: all clean install build
all: build doc

NAME=message_switch
J=4

export OCAMLRUNPARAM=b

TESTS ?= --enable-tests
ifneq "$(MIRAGE_OS)" ""
TESTS := --disable-tests
endif

clean:
	@rm -f setup.data setup.log setup.bin
	@rm -rf _build

distclean: clean
	@rm -f config.mk

-include config.mk

setup.ml: _oasis
	@oasis setup

setup.bin: setup.ml
	@ocamlopt.opt -o $@ $< || ocamlopt -o $@ $< || ocamlc -o $@ $<
	@rm -f setup.cmx setup.cmi setup.o setup.cmo

setup.data: setup.bin
	./setup.bin -configure $(TESTS) $(ASYNC) $(LWT)

build: setup.data setup.bin
	@./setup.bin -build -j $(J)

doc: setup.data setup.bin
	@./setup.bin -doc -j $(J)

OCAML := $(shell ocamlc -where)
PYTHON := $(OCAML)/../python

install: setup.bin
	@./setup.bin -install
	mkdir -p $(DESTDIR)/$(BINDIR)
	install _build/cli/main.native $(DESTDIR)/$(BINDIR)/ms
	install _build/switch/switch_main.native $(DESTDIR)/$(BINDIR)/message-switch

# oasis bug?
#test: setup.bin build
#	@./setup.bin -test
test:
	./basic-rpc-test.sh


reinstall: setup.bin
	@ocamlfind remove $(NAME) || true
	@./setup.bin -reinstall
	mkdir -p $(DESTDIR)/$(BINDIR)
	install _build/cli/main.native $(DESTDIR)/$(BINDIR)/ms
	install _build/switch/switch_main.native $(DESTDIR)/$(BINDIR)/message-switch

release:
	# Remove our dependencies on oasis and bisect
	sed -i -r s'/, bisect//g' _oasis
	sed -i -r s'/\"bisect\"//g' opam
	sed -i -r s'/\"oasis\"//g' opam
	sed -i -r s'/\"ocveralls\"//g' opam
	# Switch off -warn-error
	grep -v 'warn-error' _oasis > _oasis.tmp
	mv _oasis.tmp _oasis
	# Remove our aversion to OASIS autogen
	sed -i -r s'/setup.ml//g' .gitignore
	sed -i -r s'/myocamlbuild.ml//g' .gitignore
	sed -i -r s'/_tags//g' .gitignore
	sed -i -r s'/\*.mllib//g' .gitignore
	sed -i -r s'/\*.mldylib//g' .gitignore
	sed -i -r s'/\*.mlpack//g' .gitignore
	sed -i -r s'/META//g' .gitignore
	oasis setup
