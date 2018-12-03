.PHONY: build clean release test reindent install uninstall

build:
	dune build @install

clean:
	dune clean

release:
	jbuilder build @install  --profile=release

install:
	dune install --profile=release

uninstall:
	dune uninstall --profile=release

test:
	dune runtest --no-buffer --profile=release

reindent:
	git ls-files '*.ml*' | xargs ocp-indent --syntax cstruct -i

#requires odoc
doc:
	dune build @doc --profile=release

.DEFAULT_GOAL := release
