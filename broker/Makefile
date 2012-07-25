
all: pdf html

pdf: broker.pdf
html: broker.html

INPUTS=README.md

broker.tex: $(INPUTS)
	pandoc --toc --standalone $(INPUTS) -o $@

broker.html: $(INPUTS)
	pandoc --toc --standalone $(INPUTS) -f html -o $@

%.pdf: %.tex
	pdflatex $*
	pdflatex $*

%.tex: %
	pandoc --toc --standalone $* -o $@

%.html: %
	pandoc --toc --standalone $* -f html -o $@

.PHONY: clean
clean:
	rm -f *.pdf *.html *.tex *.out

