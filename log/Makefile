CC = gcc
OCAMLLOC := $(shell ocamlc -where)
CFLAGS = -Wall -fPIC -O2 -I/opt/xensource/lib/ocaml -I$(OCAMLLOC)
OCAMLC = ocamlc
OCAMLOPT = ocamlopt 

LDFLAGS = -cclib -L./

VERSION := $(shell git show-ref --head | grep -E " HEAD$$" | cut -f 1 -d " " || echo 0)
OCAMLCFLAGS = -g -dtypes -thread -I ../stdext
OCAMLOPTFLAGS = -g -dtypes -thread -I ../stdext


OCAMLABI := $(shell ocamlc -version)
OCAMLLIBDIR := $(shell ocamlc -where)
OCAMLDESTDIR ?= $(OCAMLLIBDIR)

OBJS = syslog log logs debug
INTF = log.cmi logs.cmi syslog.cmi debug.cmi
LIBS = log.cma log.cmxa

DOCDIR = /myrepos/xen-api-libs/doc

all: $(INTF) $(LIBS) $(PROGRAMS)

bins: $(PROGRAMS)

libs: $(LIBS)

log.cmxa: libsyslog_stubs.a $(foreach obj,$(OBJS),$(obj).cmx)
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -a -o $@ -cclib -lsyslog_stubs $(foreach obj,$(OBJS),$(obj).cmx)

log.cma: $(foreach obj,$(OBJS),$(obj).cmo)
	$(OCAMLC) -a -dllib dllsyslog_stubs.so -cclib -lsyslog_stubs -o $@ $(foreach obj,$(OBJS),$(obj).cmo)

syslog_stubs.a: syslog_stubs.o
	ocamlmklib -o syslog_stubs $+

libsyslog_stubs.a: syslog_stubs.o
	ar rcs $@ $+
	ocamlmklib -o syslog_stubs $+

%.cmi: %.mli
	$(OCAMLC) -c $(OCAMLCFLAGS) -o $@ $<

%.cmo: %.ml %.cmi
	$(OCAMLC) -c $(OCAMLCFLAGS) -thread -o $@ $<

%.cmx: %.ml %.cmi
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -thread -c -o $@ $<

%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $<

META: META.in
	sed 's/@VERSION@/$(VERSION)/g' < $< > $@

#dependency:
log.cmi: syslog.cmi
logs.cmi: log.cmi
debug.cmi: logs.cmi

.PHONY: install
install: path = $(DESTDIR)$(shell ocamlfind printconf destdir)
install: $(LIBS) META
	mkdir -p $(path)
	ocamlfind install -destdir $(path) -ldconf ignore log META $(INTF) $(LIBS) *.a *.so *.cmx *.sp?t

.PHONY: uninstall
uninstall:
	ocamlfind remove log

.PHONY: doc
doc: $(INTF)
	python ../doc/doc.py $(DOCDIR) "log" "package" "$(OBJS)" "." "stdext" ""

clean:
	rm -f *.o *.so *.a *.cmo *.cmi *.cma *.cmx *.cmxa *.annot *.sp?t $(LIBS) $(PROGRAMS)
