# A note on generating locally the API reference

Run `make doc`. The output is in `_build/ocaml/doc/markdown`.

To view the markdown properly you will need to install `graphviz` so as to
convert the dot files to images. To generate the API reference in other formats
(currently docbook and pdf) you will need to install `pandoc`. For pdf format
you will also need `pdflatex`.

To convert the images only, run `sh doc-convert.sh`.

To generate the docbook run `sh doc-convert.sh --docbook`.

To generate the pdf run `sh doc-convert.sh --pdf`.
