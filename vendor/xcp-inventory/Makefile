.PHONY: build clean release test reindent install uninstall

build:
	jbuilder build @install --dev

clean:
	jbuilder clean

release:
	jbuilder build @install

test:
	jbuilder runtest --no-buffer

reindent:
	git ls-files '*.ml*' | xargs ocp-indent --syntax cstruct -i

#requires odoc
doc:
	jbuilder build @doc

.DEFAULT_GOAL := release
