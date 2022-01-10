.PHONY: release check format clean

release:
	dune build -p varstored-guard @install

check:
	dune build -p varstored-guard @runtest

format:
	dune build @fmt --auto-promote
	opam lint

clean:
	dune clean
