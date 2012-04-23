include ../config.mk

OBJS = syslog debug
INTF = syslog.cmi debug.cmi
LIBS = log.cma log.cmxa

OCAMLCFLAGS += -thread -I ../stdext
OCAMLOPTFLAGS += -thread -I ../stdext

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
debug.cmi:

.PHONY: install
install: $(LIBS) META
	mkdir -p $(OCAMLDESTDIR)
	ocamlfind install -destdir $(OCAMLDESTDIR) -ldconf ignore log META $(INTF) $(LIBS) *.a *.so *.cmx $(SPOT)

.PHONY: uninstall
uninstall:
	ocamlfind remove log

.PHONY: doc
doc: $(INTF)
	python ../doc/doc.py $(DOCDIR) "log" "package" "$(OBJS)" "." "stdext" ""

clean:
	rm -f META *.o *.so *.a *.cmo *.cmi *.cma *.cmx *.cmxa *.annot $(SPOT) $(LIBS) $(PROGRAMS)
