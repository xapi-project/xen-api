include ../config.mk

IPROG=install -m 755
OCAMLLOC := $(shell ocamlc -where)

OCAML_TEST_INC = -I $(shell ocamlfind query oUnit)
OCAML_TEST_LIB = $(shell ocamlfind query oUnit)/oUnit.cmxa

STDEXT_OBJS = \
	monad \
	fun \
	opt \
	listext \
	filenameext \
	stringext \
	arrayext \
	hashtblext \
	pervasiveext \
	threadext \
	int64ext \
	ring \
	qring \
	fring \
	bigbuffer \
	unixext \
	range \
	vIO \
	trie \
	config \
	date \
	encodings \
	fe \
	fecomms \
	forkhelpers \
	gzip \
	sha1sum \
	zerocheck \
	base64 \
	backtrace \
	tar \
	mapext \
	os \
	either \
	lazyList \
	extentlistSet \
	set_test

INTF = $(foreach obj, $(STDEXT_OBJS),$(obj).cmi)
LIBS = stdext.cma stdext.cmxa

PROGRAMS = base64pp fe_cli fe_test extentlistset_test

all: $(INTF) $(LIBS)

bins: $(PROGRAMS)

libs: $(LIBS)

base64pp: base64_main.ml all libstdext_stubs.a
	$(OCAMLOPT) $(OCAMLOPTFLAGS) stdext.cmxa -linkpkg -o $@ $< -ccopt -L.

fe_cli: fe_cli.ml all libstdext_stubs.a
	$(OCAMLOPT) $(OCAMLOPTFLAGS) unix.cmxa ../uuid/uuid.cmxa ../rpc-light/rpc.cmx ../rpc-light/jsonrpc.cmx stdext.cmxa -linkpkg -o $@ $< -ccopt -L.

fe_test: fe_test.ml all libstdext_stubs.a
	$(OCAMLOPT) $(OCAMLOPTFLAGS) unix.cmxa ../uuid/uuid.cmxa ../rpc-light/rpc.cmx ../rpc-light/jsonrpc.cmx stdext.cmxa -linkpkg -I ../uuid -o $@ $< -ccopt -L.

extentlistset_test: extentlistset_test.ml all libstdext_stubs.a
	$(OCAMLOPT) $(OCAMLOPTFLAGS) unix.cmxa stdext.cmxa -linkpkg -o $@ $< -ccopt -L.

stdext.cmxa: libstdext_stubs.a $(foreach obj,$(STDEXT_OBJS),$(obj).cmx)
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -a -o $@ -cclib -lstdext_stubs $(foreach obj,$(STDEXT_OBJS),$(obj).cmx)

stdext.cma: $(foreach obj,$(STDEXT_OBJS),$(obj).cmo)
	$(OCAMLC) -a -dllib dllstdext_stubs.so -cclib -lstdext_stubs -o $@ $(foreach obj,$(STDEXT_OBJS),$(obj).cmo)

stdext_stubs.a: unixext_stubs.o zerocheck_stub.o 
	ocamlmklib -o stdext_stubs $+

libstdext_stubs.a: unixext_open_stubs.o unixext_write_stubs.o unixext_stubs.o zerocheck_stub.o
	ar rcs $@ $+
	ocamlmklib -o stdext_stubs $+

querycd: querycd.cmo 
	 ocamlfind ocamlc -custom -thread -package "threads,unix,stdext" querycd.cmo -o querycd -linkpkg -linkall
#camlc unix.cma stdext.cma querycd.cmo -o querycd

## OBJS
threadext.cmo: threadext.ml threadext.cmi
	$(OCAMLC) -thread -c -o $@ $<

fecomms.cmo : fecomms.ml
	$(OCAMLC) -I ../rpc-light -c -o $@ $<

fe.cmo: fe.ml 
	$(OCAMLC) $(RPCLIGHTFLAGS) -I ../jsonrpc -I ../rpc-light -c -o $@ $<

forkhelpers.cmo: forkhelpers.ml forkhelpers.cmi
	$(OCAMLC) -thread -I ../uuid -c -o $@ $<

filenameext.cmo: filenameext.ml filenameext.cmi
	$(OCAMLC) -c -I ../uuid -o $@ $<

%.cmo: %.ml %.cmi
	$(OCAMLC) -c -o $@ $<

threadext.cmi: threadext.mli
	$(OCAMLC) -thread -c -o $@ $<

forkhelpers.cmi: forkhelpers.mli
	$(OCAMLC) -thread -c -o $@ $<

filenameext.cmi: filenameext.mli
	$(OCAMLC) -c -I ../uuid -o $@ $<

fe.cmi: fe.cmo
	$(OCAMLC) $(RPCLIGHTFLAGS) -c -o $@ $<

%.cmi: %.mli
	$(OCAMLC) -c -o $@ $<

fe.cmx: fe.ml 
	$(OCAMLOPT) $(RPCLIGHTFLAGS) -I ../rpc-light -c -o $@ $<

threadext.cmx: threadext.ml threadext.cmi
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -thread -c -o $@ $<

fecomms.cmx : fecomms.ml
	$(OCAMLOPT) -I ../rpc-light -c -o $@ $<

forkhelpers.cmx: forkhelpers.ml forkhelpers.cmi
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -I ../uuid -thread -c -o $@ $<

filenameext.cmx: filenameext.ml filenameext.cmi
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -thread -c -I ../uuid -o $@ $<

%.cmx: %.ml %.cmi
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -I ../uuid -c -o $@ $<

%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $<

META: META.in
	sed 's/@VERSION@/$(VERSION)/g' < $< > $@

.PHONY: install
install: $(LIBS) META
	mkdir -p $(OCAMLDESTDIR)
	ocamlfind install -destdir $(OCAMLDESTDIR) -ldconf ignore stdext META $(INTF) $(LIBS) *.a *.so *.cmx $(SPOT)

.PHONY: bininstall
bininstall: path = $(DESTDIR)$(LIBEXEC)
bininstall: all
	mkdir -p $(path)
	$(IPROG) $(PROGRAMS) $(path)

.PHONY: uninstall
uninstall:
	ocamlfind remove stdext

.PHONY: binuninstall
binuninstall:
	rm -f $(DESTDIR)$(LIBEXEC)$(PROGRAMS)

.PHONY: doc
doc: $(INTF)
	python ../doc/doc.py $(DOCDIR) "stdext" "package" "$(STDEXT_OBJS)" "." "threads,uuid,unix" ""

clean:
	rm -f META *.o *.so *.a *.cmo *.cmi *.cma *.cmx *.cmxa *.annot $(SPOT) $(LIBS) $(PROGRAMS)

#dependency:

unixext.cmi: filenameext.cmi
gzip.cmi: forkhelpers.cmi
sha1sum.cmi: forkhelpers.cmi
tar.cmi: bigbuffer.cmi
