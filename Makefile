default: test.js

test.byte : test.cmo post.cmo client.cmo aPI.cmo xMLRPC.cmo date.cmo xml.cmo api_errors.cmo
	ocamlfind ocamlc -package js_of_ocaml,js_of_ocaml.syntax,xmlm -linkpkg -syntax camlp4o -annot -o test.byte post.cmo api_errors.cmo xml.cmo date.cmo xMLRPC.cmo aPI.cmo client.cmo test.cmo

%.cmo : %.ml
	ocamlfind ocamlc -package js_of_ocaml,js_of_ocaml.syntax,xmlm -syntax camlp4o -annot -c $<

test.js : test.byte
	js_of_ocaml test.byte

.PHONY: clean

clean : 
	rm -f *.cmo *.cmi *.byte *.js *~ *.annot

test.cmo: client.cmo post.cmo
test.cmx: client.cmx post.cmx
post.cmo:
post.cmx:
api_errors.cmo:
api_errors.cmx:
aPI.cmo: xml.cmo xMLRPC.cmo date.cmo api_errors.cmo
aPI.cmx: xml.cmx xMLRPC.cmx date.cmx api_errors.cmx
client.cmo: xml.cmo xMLRPC.cmo api_errors.cmo aPI.cmo
client.cmx: xml.cmx xMLRPC.cmx api_errors.cmx aPI.cmx
date.cmo:
date.cmx:
xml.cmo:
xml.cmx:
xMLRPC.cmo: xml.cmo date.cmo
xMLRPC.cmx: xml.cmx date.cmx
