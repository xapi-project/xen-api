# vim: set noet ts=8:
#
# This Makefile is not called from Opam but only used for
# convenience during development
#

JB 	= jbuilder

all:
	$(JB) build @install -j $$(getconf _NPROCESSORS_ONLN)

install:
	$(JB) install

clean:
	$(JB) clean
