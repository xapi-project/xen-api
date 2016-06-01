#!/bin/bash
set -e

make html
rm -fr /tmp/doc
rsync -av doc/gen/ /tmp/doc/
git checkout gh-pages
mv .git /tmp/doc/
rsync --delete -av /tmp/doc/ .

