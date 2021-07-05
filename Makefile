include config.mk

.PHONY: release build install uninstall clean test doc format

release:
	dune build @install --profile=release

build:
	dune build @install

install:
	dune install --destdir=$(DESTDIR)

uninstall:
	dune uninstall

clean:
	dune clean

test:
	dune runtest --no-buffer --profile=release

test-quick:
	dune build @runtest-quick  --profile=$(PROFILE)

# requires odoc
doc:
	dune build @doc --profile=release

format:
	dune build @fmt --auto-promote

.DEFAULT_GOAL := release
