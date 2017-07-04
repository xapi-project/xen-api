#!/bin/bash

set -x

GRAPHVIZ_FLAGS="-Tpng"

dot vm-lifecycle.dot ${GRAPHVIZ_FLAGS} -o img-vm-lifecycle
dot classes.dot ${GRAPHVIZ_FLAGS} -o img-classes

if  [ ! "$1" = "--pdf" ]; then
    exit 0
fi

if [ -f logo.eps ]; then
    epstopdf logo.eps --outfile=logo.pdf
fi

FILES="
    basics.md
    wire-protocol.md
    vm-lifecycle.md
    api-ref-autogen.md
    cover.yaml
"

PANDOC_FLAGS="
    --latex-engine=pdflatex
    --number-sections
    --chapters
"

pandoc ${FILES} ${PANDOC_FLAGS} -o xenserver-management-api-guide.pdf
