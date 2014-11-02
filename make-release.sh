#!/bin/sh

# check I'm not on the master branch
if [ -n "$(git branch | grep '* master')" ]; then
  echo "Don't run this from the master branch"
  exit 1
fi

# Disable warn-error
grep -v "warn-error +a" _oasis > _oasis.tmp
mv _oasis.tmp _oasis
oasis setup

git commit -s -m 'Disable warn-error' _oasis
git commit -a -s -m 'Regenerate OASIS'
