#!/bin/bash

# Tag the master and debian branch HEADs with the version in ./VERSION,
# according to format specified in ./debian/gbp.conf

VERSION=$(cat VERSION | tr -d '\n')

# Retag master and debian
git tag -f -a master/${VERSION} master -m "Tagged master branch with version $version"
#git tag -f -a debian/${VERSION} debian -m "Tagged debian branch with version $version"
