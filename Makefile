.PHONY: release
release:
	dune build -p varstored-guard @install

check:
	dune build -p varstored-guard @runtest

clean:
	dune clean
