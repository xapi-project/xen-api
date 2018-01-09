include config.mk

OPAM_PREFIX=$(DESTDIR)$(shell opam config var prefix)
OPAM_LIBDIR=$(DESTDIR)$(shell opam config var lib)
FLAGS=--prefix=$(OPAM_PREFIX) --libdir=$(OPAM_LIBDIR)

.PHONY: release build install uninstall clean test doc reindent

release:
	jbuilder build @install

build:
	jbuilder build @install --dev

install:
	jbuilder install $(FLAGS) message-switch-core
	jbuilder install $(FLAGS) message-switch-unix
ifeq ($(ASYNC),--enable-async)
	jbuilder install $(FLAGS) message-switch-async
endif
ifeq ($(LWT),--enable-lwt)
	jbuilder install $(FLAGS) message-switch-lwt
	install -D _build/install/default/bin/message-cli $(DESTDIR)$(SBINDIR)/message-switch
endif
	install -D _build/install/default/bin/message-cli $(DESTDIR)$(SBINDIR)/message-cli

uninstall:
	jbuilder uninstall $(FLAGS) message-switch-core
	jbuilder uninstall $(FLAGS) message-switch-unix
ifeq ($(ASYNC),--enable-async)
	jbuilder uninstall $(FLAGS) message-switch-async
endif
ifeq ($(LWT),--enable-lwt)
	jbuilder uninstall $(FLAGS) message-switch-lwt
	rm -f $(DESTDIR)$(SBINDIR)/message-switch
endif
	rm -f $(DESTDIR)$(SBINDIR)/message-cli

clean:
	jbuilder clean

test:
	jbuilder runtest

# requires odoc
doc:
	jbuilder build @doc

reindent:
	git ls-files '*.ml*' | xargs ocp-indent --syntax cstruct -i


.DEFAULT_GOAL := release
