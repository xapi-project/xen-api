#!/bin/bash

set -xeu

GRAPHVIZ_FLAGS="-Tpng"

dot vm-lifecycle.dot ${GRAPHVIZ_FLAGS} -o vm-lifecycle.png
dot classes.dot ${GRAPHVIZ_FLAGS} -o classes.png -Tcmapx -o classes.map
cp classes.png html
cp classes.png vm-lifecycle.png markdown
sed -e '/@graph_placeholder@/{r classes.map' -e 'd}' -i html/index.html
