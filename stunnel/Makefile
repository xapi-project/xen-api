include ../config.mk





OBJS = stunnel stunnel_cache
INTF = $(foreach obj, $(OBJS),$(obj).cmi)
LIBS = stunnel.cma stunnel.cmxa


all: $(INTF) $(LIBS) $(PROGRAMS)

bins: $(PROGRAMS)

libs: $(LIBS)

stunnel.cmxa: $(foreach obj,$(OBJS),$(obj).cmx)
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -a -o $@ $(foreach obj,$(OBJS),$(obj).cmx)

stunnel.cma: $(foreach obj,$(OBJS),$(obj).cmo)
	$(OCAMLC) -a -o $@ $(foreach obj,$(OBJS),$(obj).cmo)

%.cmo: %.ml
	$(OCAMLC) -c -I ../stdext -I ../uuid -I ../log -o $@ $<

%.cmi: %.mli
	$(OCAMLC) -c -I ../stdext -I ../uuid -o $@ $<

%.cmx: %.ml
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c -I ../stdext -I ../uuid -I ../log -o $@ $<

%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $<

META: META.in
	sed 's/@VERSION@/$(VERSION)/g' < $< > $@

.PHONY: install
install: path = $(DESTDIR)$(shell ocamlfind printconf destdir)
install: $(LIBS) META
	mkdir -p $(path)
	ocamlfind install -destdir $(path) -ldconf ignore stunnel META $(INTF) $(LIBS) *.a *.cmx *.sp?t

.PHONY: uninstall
uninstall:
	ocamlfind remove stunnel

.PHONY: doc
doc: $(INTF)
	python ../doc/doc.py $(DOCDIR) "stunnel" "package" "$(OBJS)" "." "stdext,log" ""

clean:
	rm -f *.o *.so *.a *.cmo *.cmi *.cma *.cmx *.cmxa *.annot *.sp?t $(LIBS) $(PROGRAMS)
