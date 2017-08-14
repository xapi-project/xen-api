#!/bin/bash

set -xeu

GRAPHVIZ_FLAGS="-Tpng"

dot vm-lifecycle.dot ${GRAPHVIZ_FLAGS} -o vm-lifecycle.png
dot classes.dot ${GRAPHVIZ_FLAGS} -o classes.png -Tcmapx -o classes.map
cp classes.png html
cp classes.png vm-lifecycle.png markdown
sed -e '/@graph_placeholder@/{r classes.map' -e 'd}' -i html/index.html

usage() {
    echo "Usage:"
    echo "    sh $0 [--pdf|--docbook]"
}

if [ "$#" -lt 1 ]; then
    exit 0
elif [ "$#" -gt 1 ]; then
    usage
    exit 1
fi

OUTNAME=xenserver-management-api-guide

FILES="
    basics.md
    wire-protocol.md
    vm-lifecycle.md
    api-ref-autogen.md
"

PDF_FLAGS="
    --latex-engine=pdflatex
    --number-sections
    --chapters
    --output=${OUTNAME}.pdf
"

DB_FLAGS="
    --to=docbook
    --output=${OUTNAME}.db
    --template=template.db
"

cd markdown

case "$1" in
    --pdf)
        pandoc ${FILES} cover.yaml ${PDF_FLAGS}
        ;;
    --docbook)
        pandoc ${FILES} ${DB_FLAGS}
        ;;
    *)
        usage
        exit 1
esac
