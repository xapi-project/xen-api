INSTALL_ARGS := $(if $(PREFIX),--prefix $(PREFIX),)

.PHONY: all
all:
	jbuilder build @install

.PHONY: install
install:
	jbuilder install $(INSTALL_ARGS)

.PHONY: uninstall
uninstall:
	jbuilder uninstall $(INSTALL_ARGS)

.PHONY: reinstall
reinstall:
	$(MAKE) uninstall
	$(MAKE) install

.PHONY: test
test:
	jbuilder runtest

.PHONY: clean
clean:
	jbuilder clean

.PHONY: all-supported-ocaml-versions
all-supported-ocaml-versions:
	jbuilder build --workspace jbuild-workspace.dev
