include ../config.mk

OBJS = lwt_mux nbd_lwt nbd
INTF = $(foreach obj, $(OBJS),$(obj).cmi)
LIBS = nbd.cma nbd.cmxa nbd_lwt.cma nbd_lwt.cmxa
FLAGS = -package bitstring.syntax,lwt -syntax camlp4o -I ../stdext 
LWTFLAGS = -package bitstring,lwt,lwt.syntax,lwt.unix -I ../stdext -I ../uuid -syntax camlp4o
PROGRAMS = nbd_test

all: $(INTF) $(LIBS) $(PROGRAMS)

bins: $(PROGRAMS)

libs: $(LIBS)

nbd_test: nbd.cmxa nbd_lwt.cmxa nbd_test.ml
	$(OCAMLFIND) $(OCAMLOPT) $(LWTFLAGS) -o $@ -linkpkg uuid.cmxa stdext.cmxa $^

nbd.cmxa: nbd.cmx
	$(OCAMLOPT) $(FLAGS) -a -o $@ nbd.cmx

nbd.cma: nbd.cmo
	$(OCAMLC) $(FLAGS) -a -o $@ nbd.cmo

nbd_lwt.cmxa: lwt_mux.cmx nbd_lwt.cmx
	$(OCAMLOPT) $(LWTFLAGS) -a -o $@ $^

nbd_lwt.cma: lwt_mux.cmo nbd_lwt.cmo
	$(OCAMLC) $(LWTFLAGS) -a -o $@ $^

lwt_mux.cmx : lwt_mux.ml
	$(OCAMLFIND) $(OCAMLOPT) -c $(LWTFLAGS) -o $@ $<

lwt_mux.cmo : lwt_mux.ml
	$(OCAMLFIND) $(OCAMLC) -c $(LWTFLAGS) -o $@ $<

lwt_mux.cmi : lwt_mux.ml
	$(OCAMLFIND) $(OCAMLC) -c $(LWTFLAGS) lwt_mux.ml

nbd_lwt.cmo : nbd.cmi nbd_lwt.ml
	$(OCAMLFIND) $(OCAMLC) -c $(LWTFLAGS) nbd_lwt.ml

nbd_lwt.cmx : nbd.cmi nbd_lwt.ml
	$(OCAMLFIND) $(OCAMLOPT) -c $(LWTFLAGS) nbd_lwt.ml

nbd_lwt.mli : nbd.cmi

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
