INSTALL_ARGS := $(if $(PREFIX),--prefix $(PREFIX),)

# Default rule
default:
	jbuilder build @install

install:
	jbuilder install $(INSTALL_ARGS)

uninstall:
	jbuilder uninstall $(INSTALL_ARGS)

reinstall: uninstall reinstall

all-supported-ocaml-versions:
	jbuilder build @install @runtest --workspace jbuild-workspace.dev

clean:
	jbuilder clean

test:
	jbuilder runtest

promote:
	jbuilder promote

.PHONY: default install uninstall reinstall clean test
