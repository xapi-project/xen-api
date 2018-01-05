# SUMMARY:
# Builds and tests xapi using xenserver-build-env, which installs the
# dependencies as RPMs.

set -uex

export CONTAINER_NAME=build-env
export OCAMLRUNPARAM=b
export REPO_PACKAGE_NAME=xapi
export REPO_CONFIGURE_CMD=./configure
export REPO_BUILD_CMD=make
export REPO_TEST_CMD='make test >test.log || { tail test.log; grep E: test.log; }'
export REPO_DOC_CMD='make doc-json'

wget https://raw.githubusercontent.com/xenserver/xenserver-build-env/master/utils/travis-build-repo.sh

# only run deploy.sh when the build succeeds
bash travis-build-repo.sh && \
  ( ( test $TRAVIS_PULL_REQUEST = "false" && test $TRAVIS_BRANCH = "master" && bash deploy.sh ) || true )
