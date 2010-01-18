
CC = gcc
CFLAGS = -Wall -fPIC -O2 -I/opt/xensource/lib/ocaml
LDFLAGS = -cclib -L./
VERSION = 0.1

PP = camlp4o -I ../rpc-light -I $(shell ocamlfind query type-conv) pa_type_conv.cmo pa_rpc.cma

DESTDIR ?= /
VERSION := $(shell hg parents --template "{rev}" 2>/dev/null || echo 0.0)

OCAMLABI := $(shell ocamlc -version)
OCAMLLIBDIR := $(shell ocamlc -where)
OCAMLDESTDIR ?= $(OCAMLLIBDIR)

LIBOBJS = constants lvm_uuid crc utils absty lvmconfigparser lvmconfiglex lvmmarshal allocator debug redo lv pv vg 
INTF = $(foreach obj, $(LIBOBJS),$(obj).cmi)
CMDOBJS = messages.cmx mlvm.cmx
OCAMLC = ocamlfind ocamlc -g
OCAMLOPT = ocamlfind ocamlopt 
COMPFLAG = -dtypes -g -I ../stdext -I ../camldm -I ../uuid -for-pack Lvm -pp '${PP}' -I ../rpc-light

LIBS = lvm.cma lvm.cmxa

DOCDIR = /myrepos/xen-api-libs.hg/doc

default : $(LIBS)

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
	ocamlfind install -destdir $(DESTDIR)$(shell ocamlfind printconf destdir) -ldconf ignore lvm META lvm.cmi lvm.cmxa lvm.cma lvm.a

.PHONY: uninstall
uninstall:
	ocamlfind remove lvm

clean : 
	rm -f *.cmo *.cmi *.cmx *.o *~ *.annot lvmconfiglex.ml \
	lvmconfigparser.mli lvmconfigparser.ml

.PHONY: doc
doc: $(INTF)
	python ../doc/doc.py $(DOCDIR) "mlvm" "package" "$(LIBOBJS)" "." "stdext,camldm,uuid,unix" ""
	
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

