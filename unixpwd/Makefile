PROFILE=release

USER = unixpwd

.PHONY: build install uninstall clean test doc reindent

build:
	dune build @install --profile=$(PROFILE)

install:
	dune install

uninstall:
	dune uninstall

clean:
	dune clean

test: build
	sudo useradd $(USER)
	sudo ./_build/default/test/main.exe $(USER)
	sudo userdel $(USER)

# requires odoc
doc:
	dune build @doc --profile=$(PROFILE)

reindent:
	git ls-files '*.ml' '*.mli' | xargs ocp-indent --inplace
