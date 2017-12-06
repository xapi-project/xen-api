
.PHONY: release build install uninstall test clean reindent

build:
	jbuilder build @install --dev

release:
	jbuilder build @install

install:
	jbuilder install

uninstall:
	jbuilder uninstall

test:
	jbuilder runtest

clean:
	jbuilder clean

reindent:
	git ls-files '**/*.ml' | xargs ocp-indent --inplace

