#!/bin/bash

# Make sure we're not echoing any sensitive data
set +x
set -o errexit -o nounset

if [ "$TRAVIS_BRANCH" != "master" ]
then
  echo "This commit was made against the $TRAVIS_BRANCH and not the master! No deploy!"
  exit 0
fi

# Error out if $GH_TOKEN is empty or unset
: ${GH_TOKEN:?"GH_TOKEN needs to be uploaded via travis-encrypt"}

# Print a hash of the token for debugging
echo Token MD5: $(echo $GH_TOKEN | md5sum)

rev=$(git rev-parse --short HEAD)

# Copy data we're interested in out of the container
docker cp ${CONTAINER_NAME}:/home/builder/xen-api/_build/install/default/jekyll/xenapi.json $HOME/
docker cp ${CONTAINER_NAME}:/home/builder/xen-api/_build/install/default/jekyll/release_info.json $HOME/

# Go to home and setup git
cd $HOME
git config --global user.email "travis@travis-ci.org"
git config --global user.name "Travis"

# Using token clone xapi-project.github.io.git
git clone "https://$GH_TOKEN@github.com/xapi-project/xapi-project.github.io.git" 2>&1 | sed -e "s/$GH_TOKEN/!REDACTED!/g"

# Copy data we're interested in into the right place
cd xapi-project.github.io
cp -f $HOME/xenapi.json _data/
cp -f $HOME/release_info.json _data/

git commit -am "Updated XenAPI docs based on xen-api/${rev}"
git push origin master 2>&1 | sed -e "s/$GH_TOKEN/!REDACTED!/g"
