include config.mk

.PHONY: release build install uninstall clean test doc reindent

release:
	dune build @install --profile=release

build:
	dune build @install

install:
	dune install message-switch-core
	dune install message-switch-unix
ifeq ($(ASYNC),--enable-async)
	dune install message-switch-async
endif
ifeq ($(LWT),--enable-lwt)
	dune install message-switch-lwt
	install -D _build/install/default/bin/message-switch $(DESTDIR)$(SBINDIR)
endif
	install -D _build/install/default/bin/message-cli $(DESTDIR)$(SBINDIR)

uninstall:
	dune uninstall message-switch-core
	dune uninstall message-switch-unix
ifeq ($(ASYNC),--enable-async)
	dune uninstall message-switch-async
endif
ifeq ($(LWT),--enable-lwt)
	dune uninstall message-switch-lwt
	rm -f $(DESTDIR)$(SBINDIR)/message-switch
endif
	rm -f $(DESTDIR)$(SBINDIR)/message-cli

clean:
	dune clean

test:
	dune runtest --no-buffer --profile=release

# requires odoc
doc:
	dune build @doc --profile=release

reindent:
	git ls-files '*.ml*' | xargs ocp-indent --syntax cstruct -i


.DEFAULT_GOAL := release
