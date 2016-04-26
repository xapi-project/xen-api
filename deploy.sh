#!/bin/bash

set -o errexit -o nounset

if [ "$TRAVIS_BRANCH" != "master" ]
then
  echo "This commit was made against the $TRAVIS_BRANCH and not the master! No deploy!"
  exit 0
fi

rev=$(git rev-parse --short HEAD)

# Copy data we're interested in out of the container
docker cp ${CONTAINER}:${REPO}/ocaml/idl/json_backend/xenapi.json $HOME/
docker cp ${CONTAINER}:${REPO}/ocaml/idl/json_backend/release_info.json $HOME/

# Go to home and setup git
cd $HOME
git config --global user.email "travis@travis-ci.org"
git config --global user.name "Travis"

# Using token clone xapi-project.github.io.git
git clone --quiet "https://$GH_TOKEN@github.com/xapi-project/xapi-project.github.io.git" > /dev/null

# Copy data we're interested in into the right place
cd xapi-project.github.io.git
cp -f $HOME/xenapi.json _data/
cp -f $HOME/release_info.json _data/

git commit -am "Updated XenAPI docs based on xen-api/${rev}"
git push -q origin master > /dev/null
