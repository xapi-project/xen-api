include ../config.mk

IPROG=install -m 755

INIT_D   = /etc/rc.d/init.d

OBJS = 
INTF = $(foreach obj, $(OBJS),$(obj).cmi)
LIBS = 

PROGRAMS = fe

all: $(INTF) $(LIBS) $(PROGRAMS)

bins: $(PROGRAMS)

libs: $(LIBS)

test_forker: test_forker.cmx
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -I ../rpc-light -I ../uuid -I ../stdext uuid.cmxa rpc.cmx jsonrpc.cmx -I ../log unix.cmxa stdext.cmxa  test_forker.cmx -o $@

fe: fe_debug.cmx child.cmx fe_main.cmx
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -I ../rpc-light -I ../stdext -I ../uuid  -I ../log log.cmxa uuid.cmxa unix.cmxa rpc.cmx jsonrpc.cmx stdext.cmxa fe_debug.cmx child.cmx fe_main.cmx -o $@

%.cmo: %.ml
	$(OCAMLC) -c -I ../log -I ../uuid -I ../stdext -thread -o $@  $<

%.cmi: %.mli
	$(OCAMLC) -c -I ../log -I ../uuid -I ../stdext -o $@  $<

%.cmx: %.ml
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -I ../log -I ../uuid -c -I ../stdext -o $@ $<

META: META.in
	sed 's/@VERSION@/$(VERSION)/g' < $< > $@

.PHONY: install
install: 

.PHONY: bininstall
bininstall: path = $(DESTDIR)$(LIBEXEC)
bininstall: all
	mkdir -p $(DESTDIR)$(LIBEXEC)
	$(IPROG) $(PROGRAMS) $(DESTDIR)$(LIBEXEC)
	mkdir -p $(DESTDIR)$(INIT_D)
	$(IPROG) init.d-fe $(DESTDIR)$(INIT_D)/fe

.PHONY: uninstall
uninstall:

.PHONY: binuninstall
binuninstall:
	rm -f $(DESTDIR)$(LIBEXEC)$(PROGRAMS)
	rm -f $(DESTDIR)$(INIT_D)/fe

.PHONY: doc
doc: 

clean:
	rm -f *.o *.so *.a *.cmo *.cmi *.cma *.cmx *.cmxa *.annot $(SPOT) $(LIBS) $(PROGRAMS)
