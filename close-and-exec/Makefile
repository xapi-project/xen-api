include ../config.mk

IPROG=install -m 755 


OBJS = closeandexec
INTF = $(foreach obj, $(OBJS),$(obj).cmi)
LIBS = closeandexec.cmxa closeandexec.cma

PROGRAMS = closeandexec

all: $(INTF) $(LIBS)

bins: $(PROGRAMS)

libs: $(LIBS)

closeandexec: closeandexec.cmxa closeandexec_main.cmx all
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -thread -I ../stdext -I ../uuid uuid.cmxa unix.cmxa threads.cmxa stdext.cmxa closeandexec.cmxa closeandexec_main.cmx -o $@

closeandexec.cmxa: $(foreach obj,$(OBJS),$(obj).cmx)
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -a -o $@ $(foreach obj,$(OBJS),$(obj).cmx)

closeandexec.cma: $(foreach obj,$(OBJS),$(obj).cmo)
	$(OCAMLC) -a -o $@ $(foreach obj,$(OBJS),$(obj).cmo)

%.cmo: %.ml
	$(OCAMLC) -c -I ../stdext -thread -o $@ $<

%.cmi: %.mli
	$(OCAMLC) -c -I ../stdext -o $@ $<

%.cmx: %.ml
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c -I ../stdext -thread -o $@ $<

%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $<

META: META.in
	sed 's/@VERSION@/$(VERSION)/g' < $< > $@

.PHONY: install
install: $(LIBS) META
	mkdir -p $(OCAMLDESTDIR)
	ocamlfind install -destdir $(OCAMLDESTDIR) -ldconf ignore close-and-exec META $(INTF) $(LIBS) *.a *.cmx $(SPOT)

.PHONY: bininstall
bininstall: path = $(DESTDIR)$(LIBEXEC)
bininstall: all
	mkdir -p $(path)
	$(IPROG) $(PROGRAMS) $(path)

.PHONY: uninstall
uninstall:
	ocamlfind remove close-and-exec

.PHONY: binuninstall
binuninstall:
	rm -f $(DESTDIR)$(LIBEXEC)$(PROGRAMS)

.PHONY: doc
doc: $(INTF)
	python ../doc/doc.py $(DOCDIR) "close-and-exec" "library" "$(OBJS)" "." "stdext" ""

clean:
	rm -f META *.o *.so *.a *.cmo *.cmi *.cma *.cmx *.cmxa *.annot $(SPOT) $(LIBS) $(PROGRAMS)
