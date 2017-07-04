# A note on generating locally the API reference

Run `make doc`. The output is in `_build/ocaml/doc/markdown`.

To view the markdown properly you will need to install `graphviz` so as to
convert the dot files to images. To generate a pdf in addition, you will need to
install `pandoc` and `pdflatex`.

To convert the images only, run `sh doc-convert.sh`.

To also generated the pdf run `sh doc-convert.sh --pdf`.
