.PHONY: release build install uninstall clean test doc reindent

release:
	jbuilder build @install

build:
	jbuilder build @install --dev

install:
	jbuilder install

uninstall:
	jbuilder uninstall

clean:
	jbuilder clean

test:
	jbuilder runtest

# requires odoc
doc:
	jbuilder build @doc

reindent:
	git ls-files '*.ml*' | xargs ocp-indent --syntax cstruct -i


.DEFAULT_GOAL := release
