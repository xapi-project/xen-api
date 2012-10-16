default: switch

all: pdf html

pdf: broker.pdf
html: broker.html

INPUTS=README.md

broker.tex: $(INPUTS)
	pandoc --toc --standalone $(INPUTS) -o $@

broker.html: $(INPUTS)
	pandoc --toc --standalone $(INPUTS) -f html -o $@

PACKS=lwt,lwt.unix,lwt.syntax,xenstore,xenstore.server,cohttp,cohttp.lwt,rpc,rpc.json,rpc.syntax

switch: protocol.cmi protocol.cmx switch.cmx
	ocamlfind ocamlopt -package "${PACKS}" -linkpkg -o switch protocol.cmx switch.cmx

client: protocol.cmi protocol.cmx client.cmx
	ocamlfind ocamlopt -package "${PACKS}" -linkpkg -o client protocol.cmx client.cmx

server: protocol.cmi protocol.cmx server.cmx
	ocamlfind ocamlopt -package "${PACKS}" -linkpkg -o server protocol.cmx server.cmx

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

