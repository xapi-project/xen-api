#!/bin/bash
set -ex

make html
rm -fr /tmp/doc
rsync -av _build/default/doc/gen/ /tmp/doc/
git checkout gh-pages
mv .git /tmp/doc/
rsync --delete -av /tmp/doc/ .

