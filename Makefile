
idl: types.cmx smapiv2.cmx xenops.cmx memory.cmx main.cmx
	ocamlfind ocamlopt -package xmlm,stdext -linkpkg -g -o idl types.cmx smapiv2.cmx xenops.cmx memory.cmx main.cmx

toplevel: types.cmo smapiv2.cmo xenops.cmo memory.cmo
	ocamlfind ocamlmktop -thread -package xmlm,stdext -linkpkg -g -o toplevel types.cmo smapiv2.cmo xenops.cmo memory.cmo

%.cmx: %.ml
	ocamlfind ocamlopt -package xmlm,stdext -c -g $<

%.cmo: %.ml
	ocamlfind ocamlc -package xmlm,stdext -c -g $<

DESTDIR?=
PATH=/root/fs
INSTALL=/usr/bin/install
MKDIR=/bin/mkdir

.PHONY: install
install: idl
	./idl
	${MKDIR} -p ${DESTDIR}${PATH}
	${INSTALL} python/fs.py ${DESTDIR}${PATH}
	${INSTALL} python/mount.py ${DESTDIR}${PATH}
	${INSTALL} python/storage.py ${DESTDIR}${PATH}
	${INSTALL} python/tapdisk.py ${DESTDIR}${PATH}
	${INSTALL} python/util.py ${DESTDIR}${PATH}
	${INSTALL} python/vhd.py ${DESTDIR}${PATH}
	${INSTALL} python/xcp.py ${DESTDIR}${PATH}

.PHONY: python/xcp-sm-fs.spec
python/xcp-sm-fs.spec: python/xcp-sm-fs.spec.in
	sed -e 's/@RPM_RELEASE@/$(shell git rev-list HEAD | wc -l)/g' < $< > $@


.PHONY: clean
clean:
	rm -f *.cmx *.cmo idl toplevel
