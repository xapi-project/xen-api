#!/bin/sh

# How it works: We push the generated markdown files to the "slate" branch,
# whose Travis build will update the gh-pages branch which contains the actual
# website.

# Make sure we're not echoing any sensitive data
set +x

set -e

echo "Updating docs on Github pages..."

dune build generator/src/main.exe

DOCDIR=.gh-pages
if [ -n "$KEEP" ]; then trap "rm -rf $DOCDIR" EXIT; fi
rm -rf $DOCDIR

# Don't expose GH_TOKEN
git clone --quiet --branch=slate git@github.com/xapi-project/xapi-storage $DOCDIR > /dev/null 2>&1
rm -rf $DOCDIR/source/includes/*
dune exec generator/src/main.exe -- gen_markdown --path=$DOCDIR/source/includes
git -C $DOCDIR config user.email "travis@travis-ci.org"
git -C $DOCDIR config user.name "Travis"
(cd $DOCDIR; git add .)
git -C $DOCDIR commit --allow-empty -am "Travis build $TRAVIS_BUILD_NUMBER pushed docs to slate"
# Don't expose GH_TOKEN
git -C $DOCDIR push origin slate > /dev/null 2>&1
echo "Docs pushed"
