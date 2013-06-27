#!/bin/bash

set -e

if [ ! -f VERSION ]; then
    echo "Can't find VERSION file. Are you in the root of the source tree?"
    exit
fi

git tag -a -m "Bumping version and changelog" $(cat VERSION)

echo -n "Created tag: "
echo -n $(git describe --abbrev=0)
echo " - you may now run:"
echo " $ git push origin $(cat VERSION)"
echo "To push this tag"
