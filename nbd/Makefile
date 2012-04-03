include ../config.mk

OBJS = lwt_mux nbd
INTF = $(foreach obj, $(OBJS),$(obj).cmi)
LIBS = nbd.cma nbd.cmxa
FLAGS = -package bitstring.syntax,lwt -syntax camlp4o -I ../stdext 
LWTFLAGS = -package bitstring,lwt,lwt.syntax,lwt.unix -I ../stdext -I ../uuid -syntax camlp4o
PROGRAMS = nbd_test

all: $(INTF) $(LIBS) $(PROGRAMS)

bins: $(PROGRAMS)

libs: $(LIBS)

nbd_test: nbd.cmxa nbd_test.ml
	$(OCAMLFIND) $(OCAMLOPT) $(LWTFLAGS) -o $@ -linkpkg uuid.cmxa stdext.cmxa $^

nbd.cmxa: $(foreach obj,$(OBJS),$(obj).cmx)
	$(OCAMLOPT) $(FLAGS) -a -o $@ $(foreach obj,$(OBJS),$(obj).cmx)

nbd.cma: $(foreach obj,$(OBJS),$(obj).cmo)
	$(OCAMLC) $(FLAGS) -a -o $@ $(foreach obj,$(OBJS),$(obj).cmo)

lwt_mux.cmx : lwt_mux.ml
	$(OCAMLFIND) $(OCAMLOPT) -c $(LWTFLAGS) -o $@ $<

lwt_mux.cmo : lwt_mux.ml
	$(OCAMLFIND) $(OCAMLC) -c $(LWTFLAGS) -o $@ $<

lwt_mux.cmi : lwt_mux.ml
	$(OCAMLFIND) $(OCAMLC) -c $(LWTFLAGS) $<

%.cmo: %.ml
	$(OCAMLFIND) $(OCAMLC) -c $(FLAGS) -o $@ $<

%.cmi: %.mli
	$(OCAMLC) -c $(FLAGS) -o $@ $<

%.cmx: %.ml
	$(OCAMLOPT) $(FLAGS) -c -o $@ $<

%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $<

META: META.in
	sed 's/@VERSION@/$(VERSION)/g' < $< > $@

.PHONY: install
install: $(LIBS) META
	mkdir -p $(OCAMLDESTDIR)
	ocamlfind install -destdir $(OCAMLDESTDIR) -ldconf ignore nbd META $(INTF) $(LIBS) *.a *.cmx $(SPOT)

.PHONY: uninstall
uninstall:
	ocamlfind remove nbd

.PHONY: doc
doc: $(INTF)
	python ../doc/doc.py $(DOCDIR) "nbd" "package" "$(OBJS)" "." ""

clean:
	rm -f META *.o *.so *.a *.cmo *.cmi *.cma *.cmx *.cmxa *.annot $(SPOT) $(LIBS) $(PROGRAMS)
