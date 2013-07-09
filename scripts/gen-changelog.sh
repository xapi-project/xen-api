#!/bin/bash

set -e

if [ ! -f VERSION ]; then
    echo "Can't find VERSION file. Are you in the root of the source tree?"
    exit
fi

VERSION=$(cat VERSION)
DATE=$(date "+%F")
TEMPFILE=$(tempfile)

echo "${VERSION} (${DATE}):" > ${TEMPFILE}

git --no-pager log --merges --pretty=%b \
    $(git describe --abbrev=0)..HEAD \
    | sed 's/\(.*\)/ * \1/' - >> ${TEMPFILE}

echo >> ${TEMPFILE}

cat CHANGELOG >> ${TEMPFILE}

mv -f ${TEMPFILE} CHANGELOG

echo "File CHANGELOG updated. Run 'git checkout CHANGELOG' to revert."
