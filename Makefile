INSTALL_ARGS := $(if $(PREFIX),--prefix $(PREFIX),)

# Default rule
default:
	jbuilder build --dev --auto-promote @cinaps @install

install:
	jbuilder install $(INSTALL_ARGS)

uninstall:
	jbuilder uninstall $(INSTALL_ARGS)

reinstall: uninstall reinstall

test:
	jbuilder runtest --dev

clean:
	rm -rf _build

all-supported-ocaml-versions:
	jbuilder build --dev @install --workspace jbuild-workspace.dev --root .

.PHONY: default install uninstall reinstall clean test
.PHONY: all-supported-ocaml-versions
