include ../config.mk





OBJS = xsrpc
INTF = xsrpc.cmi
LIBS = xsrpc.cma xsrpc.cmxa


all: $(INTF) $(LIBS) $(PROGRAMS)

bins: $(PROGRAMS)

libs: $(LIBS)

xsrpc.cmxa: $(foreach obj,$(OBJS),$(obj).cmx)
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -a -o $@ $(foreach obj,$(OBJS),$(obj).cmx)

xsrpc.cma: $(foreach obj,$(OBJS),$(obj).cmo)
	$(OCAMLC) -a -o $@ $(foreach obj,$(OBJS),$(obj).cmo)

%.cmo: %.ml
	$(OCAMLC) -c -o $@ $<

%.cmi: %.mli
	$(OCAMLC) -c -o $@ $<

%.cmx: %.ml
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c -o $@ $<

%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $<

META: META.in
	sed 's/@VERSION@/$(VERSION)/g' < $< > $@

.PHONY: install
install: path = $(DESTDIR)$(shell ocamlfind printconf destdir)
install: $(LIBS) META
	mkdir -p $(path)
	ocamlfind install -destdir $(path) -ldconf ignore xsrpc META $(INTF) $(LIBS) *.a *.cmx *.sp?t

.PHONY: uninstall
uninstall:
	ocamlfind remove xsrpc

.PHONY: doc
doc: $(INTF)
	python ../doc/doc.py $(DOCDIR) "xsrpc" "package" "$(OBJS)" "." "xb,xs" ""

clean:
	rm -f *.o *.so *.a *.cmo *.cmi *.cma *.cmx *.cmxa *.annot *.sp?t META $(LIBS) $(PROGRAMS)

