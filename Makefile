URL ?= https://github.com/xapi-project/xen-api
BRANCH ?= master

.PHONY: build release async-examples lwt-examples install uninstall clean test doc reindent regenerate

build:
	jbuilder build @install --dev

release:
	jbuilder build @install

async-examples:
	jbuilder build @async_examples/examples

lwt-examples:
	jbuilder build @lwt_examples/examples

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

gh-pages:
	bash .docgen.sh

reindent:
	git ls-files '*.ml' '*.mli' | xargs ocp-indent --syntax cstruct -i

