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

regenerate-files:
	opam install --deps-only xapi -y
	git clone --single-branch --branch $(BRANCH) $(URL) .xapi
	# Generate and copy over files
	cd .xapi ; \
	jbuilder build _build/default/ocaml/idl/ocaml_backend/gen_api_main.exe ; \
	cp ocaml/xapi-consts/api_errors.ml   ../lib/api_errors.ml   ; \
	cp ocaml/xapi-consts/api_messages.ml ../lib/api_messages.ml ; \
	cp ocaml/xapi-client/event_helper.ml ../lib/event_helper.ml ; \
	cp ocaml/xapi-types/event_types.ml   ../lib/event_types.ml  ; \
	_build/default/ocaml/idl/ocaml_backend/gen_api_main.exe  -mode api -filterinternal true -filter closed > aPI.ml       ; \
	_build/default/ocaml/idl/ocaml_backend/gen_api_main.exe  -mode client -filterinternal true -filter closed > client.ml ; \
	cp aPI.ml    ../lib/aPI.ml    ; \
	cp client.ml ../lib/client.ml
	# Update definitions in aPI.ml and drop stdext
	python2 .update_api.py
	# cleanup
	rm -rf .xapi

regenerate: regenerate-files reindent
