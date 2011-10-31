include ../config.mk





OBJS = udev
INTF = $(foreach obj, $(OBJS),$(obj).cmi)
LIBS = udev.cma udev.cmxa


all: $(INTF) $(LIBS) $(PROGRAMS)

bins: $(PROGRAMS)

libs: $(LIBS)

udev.cmxa: libudev_stubs.a $(foreach obj,$(OBJS),$(obj).cmx)
	$(OCAMLOPT) $(OCAMLOPTFLAGS) $(OCAMLFLAGS) -a -o $@ -cclib -ludev_stubs $(foreach obj,$(OBJS),$(obj).cmx)

udev.cma: libudev_stubs.a $(foreach obj,$(OBJS),$(obj).cmo)
	$(OCAMLC) $(OCAMLFLAGS) -a -dllib dlludev_stubs.so -cclib -ludev_stubs -o $@ $(foreach obj,$(OBJS),$(obj).cmo)

udev_stubs.a: udev_stubs.o
	ocamlmklib -o udev_stubs $+

libudev_stubs.a: udev_stubs.o
	ar rcs $@ $+
	ocamlmklib -o udev_stubs $+

%.cmo: %.ml
	$(OCAMLC) $(OCAMLFLAGS) -c -o $@ $<

%.cmi: %.mli
	$(OCAMLC) $(OCAMLFLAGS) -c -o $@ $<

%.cmx: %.ml
	$(OCAMLOPT) $(OCAMLFLAGS) $(OCAMLOPTFLAGS) -c -o $@ $<

%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $<

META: META.in
	sed 's/@VERSION@/$(VERSION)/g' < $< > $@

.PHONY: install
install: $(LIBS) META
	mkdir -p $(OCAMLDESTDIR)
	ocamlfind install -destdir $(OCAMLDESTDIR) -ldconf ignore udev META $(INTF) $(LIBS) *.a *.so *.cmx $(SPOT)

.PHONY: uninstall
uninstall:
	ocamlfind remove udev 

.PHONY: doc
doc: $(INTF)
	python ../doc/doc.py $(DOCDIR) "udev" "package" "$(OBJS)" "." "" ""

clean:
	rm -f META *.o *.so *.a *.cmo *.cmi *.cma *.cmx *.cmxa *.annot $(SPOT) $(LIBS) $(PROGRAMS)

