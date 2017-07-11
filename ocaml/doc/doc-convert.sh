#!/bin/bash

set -xeu

GRAPHVIZ_FLAGS="-Tpng"

dot vm-lifecycle.dot ${GRAPHVIZ_FLAGS} -o vm-lifecycle.png
dot classes.dot ${GRAPHVIZ_FLAGS} -o classes.png

if  [ ! "$1" = "--pdf" ]; then
    exit 0
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
