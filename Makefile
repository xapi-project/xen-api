.PHONY: release build install uninstall clean test doc reindent

release:
	dune build @install --profile=release

build:
	dune build @install

install:
	install -D -m 0755 ./_build/default/src/squeezed.exe $(DESTDIR)/squeezed

uninstall:
	rm $(DESTDIR)/squeezed

clean:
	dune clean

test:
	dune runtest  --profile=release

# requires odoc
doc:
	dune build @doc --profile=release

reindent:
	git ls-files '*.ml*' | xargs ocp-indent --inplace
