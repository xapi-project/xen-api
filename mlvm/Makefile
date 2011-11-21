include ../config.mk

LIBOBJS = constants lvm_uuid crc utils tag absty lvmconfigparser lvmconfiglex lvmmarshal allocator debug redo lv pv vg 
INTF = $(foreach obj, $(LIBOBJS),$(obj).cmi)
CMDOBJS = messages.cmx mlvm.cmx
COMPFLAG = -dtypes -g -I ../stdext -I ../camldm -I ../uuid -for-pack Lvm $(RPCLIGHTFLAGS)

LIBS = lvm.cma lvm.cmxa

default : $(LIBS)

test_allocator: default
	$(OCAMLOPT) -package kaputt -linkpkg -dtypes -g  -I ../stdext -I ../camldm -I ../uuid -I +kaputt unix.cmxa ../rpc-light/rpc.cmx ../rpc-light/jsonrpc.cmx ../camldm/camldm.cmxa ../uuid/uuid.cmxa ../stdext/stdext.cmxa ./lvm.cmxa test_allocator.ml -o $@

lvm.cmx: $(foreach obj,$(LIBOBJS),$(obj).cmx)
	$(OCAMLOPT) -pack -g -o $@ $(foreach obj,$(LIBOBJS),$(obj).cmx)

lvm.cmo: $(foreach obj,$(LIBOBJS),$(obj).cmo)
	$(OCAMLC) -pack -g -o $@ $(foreach obj,$(LIBOBJS),$(obj).cmo)

lvm.cmxa: lvm.cmx
	$(OCAMLOPT) -a -g -o $@ lvm.cmx

lvm.cma: lvm.cmo
	$(OCAMLC) -a -g -o $@ lvm.cmo

META: META.in
	sed 's/@VERSION@/$(VERSION)/g' < $< > $@

.PHONY: install
install: $(LIBS) META
	ocamlfind install -destdir $(OCAMLDESTDIR) -ldconf ignore lvm META lvm.cmi lvm.cmxa lvm.cma lvm.a $(SPOT)

.PHONY: uninstall
uninstall:
	ocamlfind remove lvm

clean : 
	rm -f META *.cmo *.cmi *.cmx *.o *~ *.cma *.cmxa *.a *.annot $(SPOT) lvmconfiglex.ml \
	lvmconfigparser.mli lvmconfigparser.ml
	rm -f test_allocator

.PHONY: doc
doc: $(INTF)
	python ../doc/doc.py $(DOCDIR) "mlvm" "package" "$(LIBOBJS)" "." "rpc-light,stdext,camldm,uuid,unix" ""

lvmconfigparser.ml : lvmconfigparser.mly
	ocamlyacc lvmconfigparser.mly
	$(OCAMLOPT) $(COMPFLAG) -c lvmconfigparser.mli

lvmconfiglex.ml : lvmconfiglex.mll lvmconfigparser.mli
	ocamllex lvmconfiglex.mll

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC) $(COMPFLAG) -c $<

.mli.cmi:
	$(OCAMLOPT) $(COMPFLAG) -c $<

.ml.cmx:
	$(OCAMLOPT) $(COMPFLAG) -c $<

.c.o:
	$(OCAMLC) $(COMPFLAG) -c $<

lvmcmd.cmo: messages.cmo 
lvmcmd.cmx: messages.cmx 
lv.cmo: absty.cmo 
lv.cmx: absty.cmx 
mlvm.cmo: vg.cmo pv.cmo messages.cmo lv.cmo 
mlvm.cmx: vg.cmx pv.cmx messages.cmx lv.cmx 
pv.cmo: utils.cmo lvmmarshal.cmo lvm_uuid.cmo crc.cmo constants.cmo \
    allocator.cmo absty.cmo 
pv.cmx: utils.cmx lvmmarshal.cmx lvm_uuid.cmx crc.cmx constants.cmx \
    allocator.cmx absty.cmx 
vg.cmo: debug.cmo pv.cmo lvm_uuid.cmo lv.cmo allocator.cmo absty.cmo 
vg.cmx: debug.cmx pv.cmx lvm_uuid.cmx lv.cmx allocator.cmx absty.cmx 
redo.cmo: tag.cmi debug.cmo allocator.cmo
redo.cmx: tag.cmx debug.cmx allocator.cmx
tag.cmo: debug.cmo tag.cmi
tag.cmx: debug.cmx tag.cmi

test_allocator: default
	$(OCAMLOPT) -package kaputt -linkpkg -dtypes -g  -I ../stdext -I ../camldm -I ../uuid -I +kaputt $(INCLUDES) ./lvm.cmxa test_allocator.ml -o $@

INCLUDES = unix.cmxa ../rpc-light/rpc.cmx ../rpc-light/jsonrpc.cmx ../camldm/camldm.cmxa ../uuid/uuid.cmxa ../stdext/stdext.cmxa

test_fragment: lvm.cmxa $(LIBCMXS) test_fragment.ml
	$(OCAMLOPT) $(COMPFLAG) $(INCLUDES) $^ -o $@

tag_is_valid_test: $(LIBCMXS) tag.cmx tag_is_valid_test.ml
	$(OCAMLOPT) $(COMPFLAG) $(INCLUDES) $^ -o $@
