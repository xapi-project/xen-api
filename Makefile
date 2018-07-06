JBUILDER ?= jbuilder

all:
	@$(JBUILDER) build

test:
	@$(JBUILDER) runtest

check: test

clean:
	@$(JBUILDER) clean

.PHONY: check test all clean

.PHONY: all-supported-ocaml-versions
all-supported-ocaml-versions:
	jbuilder build @runtest --workspace jbuild-workspace.dev
