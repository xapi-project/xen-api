# A note on generating locally the API reference

Run `make doc` in the repo root. This will output the API reference in html and
markdown formats in `_build/install/default/xapi/doc`.

Both html and markdown reference images which need to be generated as a separate
step from the `.dot` files. This requires `graphviz` to be installed.

To generate the images, run `sh doc-convert.sh` in
`_build/install/default/xapi/doc`. Now you can view the API reference by opening
`_build/install/default/xapi/doc/html/index.html` in your browser.
