
all: pdf html

pdf: broker.pdf
html: broker.html

INPUTS=README.md

broker.tex: $(INPUTS)
	pandoc --toc --standalone $(INPUTS) -o $@

broker.html: $(INPUTS)
	pandoc --toc --standalone $(INPUTS) -f html -o $@

PACKS=lwt,lwt.unix,lwt.syntax,cohttp,cohttp.lwt,xenstore,xenstore.server,xenstore.client

switch: xs_transport_unix.cmx switch.cmx
	ocamlfind ocamlopt -package "${PACKS}" -linkpkg -o switch xs_transport_unix.cmx switch.cmx

%.cmx: %.ml
	ocamlfind ocamlopt -package "${PACKS}" -syntax camlp4o -c $<

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

