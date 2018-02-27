#!/bin/sh

# Make sure we're not echoing any sensitive data
set +x

set -e

opam install -y caml2html
make html

if [ -z "$TRAVIS" -o "$TRAVIS_PULL_REQUEST" != "false" -o "$TRAVIS_BRANCH" != "master" ]; then
  echo "This is not a Travis-ci build on the master branch, doing nothing..."
  exit 0
else
  echo "Updating docs on Github pages..."
fi

# Error out if $GH_TOKEN is empty or unset
if [ -z "$GH_TOKEN" ]; then
  echo "GH_TOKEN not found"
  exit 1
fi

DOCDIR=.gh-pages
if [ -n "$KEEP" ]; then trap "rm -rf $DOCDIR" EXIT; fi
rm -rf $DOCDIR

# Don't expose GH_TOKEN
git clone --quiet --branch=gh-pages https://${GH_TOKEN}@github.com/xapi-project/xapi-storage $DOCDIR > /dev/null 2>&1
git -C $DOCDIR rm -rf .
cp -r _build/default/doc/gen/* $DOCDIR
git -C $DOCDIR config user.email "travis@travis-ci.org"
git -C $DOCDIR config user.name "Travis"
(cd $DOCDIR; git add *)
git -C $DOCDIR commit --allow-empty -am "Travis build $TRAVIS_BUILD_NUMBER pushed docs to gh-pages"
# Don't expose GH_TOKEN
git -C $DOCDIR push origin gh-pages > /dev/null 2>&1
