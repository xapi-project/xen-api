#!/bin/bash
set -e

make html
pushd doc/gen
rm -f /tmp/doc
rsync -av doc/gen/ /tmp/doc/
popd
git checkout gh-pages
rsync --delete -av /tmp/doc/ .

