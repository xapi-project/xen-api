include config.mk

.PHONY: release build install uninstall clean test doc format

release:
	dune build @install --profile=release

build:
	dune build @install

install:
	dune install
ifeq ($(LWT),--enable-lwt)
	install -D _build/install/default/bin/message-switch $(DESTDIR)$(SBINDIR)
endif
	install -D _build/install/default/bin/message-cli $(DESTDIR)$(SBINDIR)

uninstall:
	dune uninstall
ifeq ($(ASYNC),--enable-async)
endif
ifeq ($(LWT),--enable-lwt)
	rm -f $(DESTDIR)$(SBINDIR)/message-switch
endif
	rm -f $(DESTDIR)$(SBINDIR)/message-cli

clean:
	dune clean

test:
	dune runtest --no-buffer --profile=release

test-quick:
	dune build @runtest-quick  --profile=$(PROFILE)

# requires odoc
doc:
	dune build @doc --profile=release

format:
	dune build @fmt --auto-promote

.DEFAULT_GOAL := release
