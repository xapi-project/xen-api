#
#
#

USER = unixpwd

.PHONY: build release install uninstall clean test doc reindent

build:
	jbuilder build @install --dev

release:
	jbuilder build @install

install:
	jbuilder install

uninstall:
	jbuilder uninstall

clean:
	jbuilder clean

test: build
	sudo useradd $(USER)
	sudo ./_build/default/test/main.exe $(USER)
	sudo userdel $(USER)

# requires odoc
doc:
	jbuilder build @doc

reindent:
	git ls-files '*.ml' '*.mli' | xargs ocp-indent --inplace
