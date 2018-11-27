.PHONY: release
release:
	dune build --profile=release @install

check:
	dune build --profile=release @runtest
