#
# This Makefile is not called from Opam but only used for
# convenience during development
#

DUNE 	= dune
PROFILE = dev

.PHONY: all install test clean format lint release

all:
	$(DUNE) build --profile=$(PROFILE)

install:
	$(DUNE) install --profile=$(PROFILE)

clean:
	$(DUNE) clean

format:
	dune build @fmt --auto-promote
	indent -linux lib/polly_stubs.c

lint:
	opam lint polly.opam
	opam lint --normalise polly.opam > polly.tmp && mv polly.tmp polly.opam

release:
	dune-release tag
	dune-release distrib
	dune-release opam pkg

# vim:ts=8:noet:
