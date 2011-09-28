include ../config.mk

#FEPP = camlp4o -I ../rpc-light -I $(shell ocamlfind query type-conv) pa_type_conv.cmo pa_rpc.cma

OBJS = tapctl
INTF = $(foreach obj, $(OBJS),$(obj).cmi)
LIBS = tapctl.cma tapctl.cmxa

OCAMLFLAGS = $(RPCLIGHTFLAGS) -I ../rpc-light -I ../stdext

all: $(INTF) $(LIBS) $(PROGRAMS)

bins: $(PROGRAMS)

libs: $(LIBS)

tapctl.cmxa: $(foreach obj,$(OBJS),$(obj).cmx)
	$(OCAMLOPT) $(OCAMLOPTFLAGS) $(OCAMLFLAGS) -a -o $@ $(foreach obj,$(OBJS),$(obj).cmx)

tapctl.cma: $(foreach obj,$(OBJS),$(obj).cmo)
	$(OCAMLC) $(OCAMLOPTFLAGS) $(OCAMLFLAGS) -a -o $@ $(foreach obj,$(OBJS),$(obj).cmo)

%.cmo: %.ml
	$(OCAMLC) $(OCAMLFLAGS) -c -o $@ $<

%.cmi: %.mli
	$(OCAMLC) $(OCAMLFLAGS) -c -o $@ $<

%.cmx: %.ml
	$(OCAMLOPT) $(OCAMLOPTFLAGS) $(OCAMLFLAGS) -c -o $@ $<

%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $<

META: META.in
	sed 's/@VERSION@/$(VERSION)/g' < $< > $@

.PHONY: install
install: path = $(DESTDIR)$(shell ocamlfind printconf destdir)
install: $(LIBS) META
	mkdir -p $(path)
	ocamlfind install -destdir $(path) -ldconf ignore tapctl META $(INTF) $(LIBS) *.a *.cmx *.sp?t

.PHONY: uninstall
uninstall:
	ocamlfind remove tapctl

.PHONY: doc
doc: $(INTF)
	python ../doc/doc.py $(DOCDIR) "tapctl" "package" "$(OBJS)" "." "" ""

clean:
	rm -f *.o *.so *.a *.cmo *.cmi *.cma *.cmx *.cmxa *.annot *.sp?t $(LIBS) $(PROGRAMS)
