default: switch

all: pdf html

pdf: broker.pdf
html: broker.html

INPUTS=README.md

broker.tex: $(INPUTS)
	pandoc --toc --standalone $(INPUTS) -o $@

broker.html: $(INPUTS)
	pandoc --toc --standalone $(INPUTS) -f html -o $@

PACKS=lwt,lwt.unix,lwt.syntax,cohttp,cohttp.lwt,xenstore,xenstore.server,xenstore.client,rpc,rpc.json,rpc.syntax

switch: xs_transport_unix.cmx protocol.cmx switch.cmx
	ocamlfind ocamlopt -package "${PACKS}" -linkpkg -o switch xs_transport_unix.cmx protocol.cmx switch.cmx

client: xs_transport_unix.cmx protocol.cmi protocol.cmx client.cmx
	ocamlfind ocamlopt -package "${PACKS}" -linkpkg -o client xs_transport_unix.cmx protocol.cmx client.cmx

server: xs_transport_unix.cmx protocol.cmi protocol.cmx server.cmx
	ocamlfind ocamlopt -package "${PACKS}" -linkpkg -o client xs_transport_unix.cmx protocol.cmx server.cmx

%.cmx: %.ml
	ocamlfind ocamlopt -package "${PACKS}" -syntax camlp4o -c $<

%.cmi: %.mli
	ocamlfind ocamlc -package "${PACKS}" -syntax camlp4o -c $<

%.pdf: %.tex
	pdflatex $*
	pdflatex $*

%.tex: %
	pandoc --toc --standalone $* -o $@

%.html: %
	pandoc --toc --standalone $* -f html -o $@

.PHONY: clean
clean:
	rm -f *.pdf *.html *.tex *.out *.cmx *.cmi switch

