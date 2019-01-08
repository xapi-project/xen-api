PROFILE=release


.PHONY: build install uninstall clean test doc reindent

build:
	dune build @install --profile=$(PROFILE)

install:
	dune install

uninstall:
	dune uninstall

clean:
	dune clean

test:
	dune runtest --profile=$(PROFILE)

# requires odoc
doc:
	dune build @doc --profile=$(PROFILE)

reindent:
	git ls-files '*.ml' '*.mli' | xargs ocp-indent --inplace
