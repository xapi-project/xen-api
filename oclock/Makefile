# Copyright (c) 2011, Mickaël Delahaye <mickael.delahaye@gmail.com>
#
# Permission to use, copy, modify, and/or distribute this software for any
# purpose with or without fee is hereby granted, provided that the above
# copyright notice and this permission notice appear in all copies.
#
# THE SOFTWARE IS PROVIDED “AS IS” AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
# REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
# AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
# INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
# LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
# OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
# PERFORMANCE OF THIS SOFTWARE.

OCAMLC ?= ocamlc
OCAMLOPT ?= ocamlopt
OCAMLDEP ?= ocamldep
OCAMLDOC ?= ocamldoc
OCAMLFIND ?= $(shell which ocamlfind > /dev/null && echo ocamlfind)

OCAMLBFLAGS ?=
OCAMLOFLAGS ?=
OCAMLDEPFLAGS ?=

OCAMLLIBDIR ?= $(shell ocamlc -where)
INSTALL_DIR ?= $(OCAMLLIBDIR)/oclock
STUBLIBS_DIR ?= $(OCAMLLIBDIR)/stublibs

################################################################################

OCAMLC += $(OCAMLBFLAGS)
OCAMLOPT += $(OCAMLOFLAGS)
OCAMLDEP += $(OCAMLDEPFLAGS)

CFLAGS ?= -fPIC
CLIBS = rt pthread
# pthread seems necessary with some build of ocaml

export OCAMLC
export OCAMLOPT
export OCAMLDEP

all: byte native

byte: oclock.cma dlloclock.so
native: oclock.cmxa liboclock.a oclock.a

# Generic library compilation
%.cma: %.cmo dll%.so
	$(OCAMLC) -dllib -l$(@:.cma=) $< -a -o $@
%.cmxa: %.cmx lib%.a
	$(OCAMLOPT) $(CLIBS:%=-cclib -l%) -cclib -l$(@:.cmxa=) $< -a -o $@

lib%.a: %_stubs.o
	ar crs $@ $<
dll%.so: %_stubs.o
	$(LD) -shared -o $@ $< -lrt

# Generic Ocaml compilation
%.cmo:%.ml
	$(OCAMLC) -c $<
%.cmi:%.mli
	$(OCAMLC) -c $<
%.cmx:%.ml
	$(OCAMLOPT) -c $<

# Dependencies
.depend:
	$(OCAMLDEP) *.ml *.mli > .depend

-include .depend

# Cleaning
clean:
	$(RM) *.cmo *.cmi *.cmx
	$(MAKE) -C examples clean

distclean: clean
	$(RM) *.cma *.cmxa *.a *.so
	$(MAKE) -C examples distclean

# (Un)Install
install: all
ifdef OCAMLFIND
	$(OCAMLFIND) install oclock oclock.cma oclock.cmxa liboclock.a oclock.cmi oclock.a META -dll dlloclock.so
else
	install -d $(INSTALL_DIR)
	install -t $(INSTALL_DIR) oclock.cma oclock.cmxa liboclock.a oclock.cmi oclock.a META
	install -t $(STUBLIBS_DIR) dlloclock.so
endif

uninstall:
ifdef OCAMLFIND
	$(OCAMLFIND) remove oclock 
else
	$(RM) -r $(INSTALL_DIR)
	$(RM) $(STUBLIBS_DIR)/dlloclock.so
endif

# Documentation

doc:
	mkdir -p doc
	$(OCAMLDOC) -d doc -html -d doc *.mli

# Examples
test examples: all
	$(MAKE) -C examples

# Phony targets
.PHONY: install clean distclean all test examples byte native doc
