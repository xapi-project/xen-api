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

.PHONY: clean
clean:
	rm -rf _build *.install
	find . -name .merlin -delete
