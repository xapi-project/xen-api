PACKS=lwt,lwt.syntax,lwt.unix,stdext
OBJS=helpers iteratees lwt_support test websockets wsproxy
OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLFIND=ocamlfind
OCAMLCFLAGS=-package $(PACKS) -syntax camlp4o -g
OCAMLOPTFLAGS=-package $(PACKS) -syntax camlp4o -annot -g
OCAMLLINKFLAGS=-package $(PACKS) -linkpkg

wsproxy : $(foreach obj,$(OBJS),$(obj).cmx)
	$(OCAMLFIND) $(OCAMLOPT) $(OCAMLLINKFLAGS) $^ -o wsproxy

%.cmo: %.ml %.cmi
	$(OCAMLFIND) $(OCAMLC) $(OCAMLCFLAGS) -c -o $@ $<

%.cmi: %.mli
	$(OCAMLFIND) $(OCAMLC) $(OCAMLCFLAGS) -c -o $@ $<

%.cmx: %.ml %.cmi
	$(OCAMLFIND) $(OCAMLOPT) $(OCAMLOPTFLAGS) $(RPCLIGHTFLAGS) -c -thread -I ../rpc-light -I ../stdext -I ../log -I ../stunnel -o $@ $<

.PHONY: clean install
clean:
	rm -f *.annot *.o *~ *.cmi *.cmx *.cmo wsproxy

install: wsproxy
	install -D wsproxy $(DESTDIR)/opt/xensource/libexec/wsproxy

RPM_SOURCESDIR ?= /usr/src/redhat/SOURCES
RPM_SRPMSDIR ?= /usr/src/redhat/SRPMS

wsproxy.spec: wsproxy.spec.in
	sed -e 's/@RPM_RELEASE@/$(shell git rev-list HEAD | wc -l)/g' < $< > $@

srpm: wsproxy.spec
	mkdir -p $(RPM_SOURCESDIR)
	git archive --prefix=wsproxy-0/ --format=tar HEAD | bzip2 -z > $(RPM_SOURCESDIR)/wsproxy.tar.bz2
	rpmbuild -bs --nodeps --define "_sourcedir ${RPM_SOURCESDIR}" --define "_srcrpmdir ${RPM_SRPMSDIR}" wsproxy.spec

helpers.cmo: helpers.cmi
helpers.cmx: helpers.cmi
iteratees.cmo: helpers.cmi iteratees.cmi
iteratees.cmx: helpers.cmx iteratees.cmi
lwt_support.cmo: iteratees.cmi lwt_support.cmi
lwt_support.cmx: iteratees.cmx lwt_support.cmi
test.cmo: iteratees.cmi test.cmi
test.cmx: iteratees.cmx test.cmi
websockets.cmo: test.cmi iteratees.cmi helpers.cmi websockets.cmi
websockets.cmx: test.cmx iteratees.cmx helpers.cmx websockets.cmi
wsproxy.cmo: websockets.cmi lwt_support.cmi wsproxy.cmi
wsproxy.cmx: websockets.cmx lwt_support.cmx wsproxy.cmi
helpers.cmi:
iteratees.cmi:
lwt_support.cmi: iteratees.cmi
test.cmi:
websockets.cmi: iteratees.cmi
wsproxy.cmi:






