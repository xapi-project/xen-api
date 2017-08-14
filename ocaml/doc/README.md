# A note on generating locally the API reference

Run `make doc` in the repo root. This will output the API reference in html and
markdown formats in `_build/ocaml/doc`.

Both html and markdown reference images which need to be generated as a separate
step from the `.dot` files. This requires `graphviz` to be installed.

To generate the images, run `sh doc-convert.sh` in `_build/ocaml/doc`. Now you
can view the API reference by opening `_build/ocaml/doc/html/index.html` in your
browser.

To generate the API reference in other formats (currently docbook and pdf) you
will need to install `pandoc`.

To generate the docbook run `sh doc-convert.sh --docbook` in `_build/ocaml/doc`.

To generate the pdf you also need to install `pdflatex` (available within
`texlive` distibutions). Then run `sh doc-convert.sh --pdf` in `_build/ocaml/doc`.
