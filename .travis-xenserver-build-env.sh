# SUMMARY:
# Builds and tests xapi using xenserver-build-env, which installs the
# dependencies as RPMs.

set -uex

wget https://raw.githubusercontent.com/xenserver/xenserver-build-env/master/utils/travis-build-repo.sh

# only run deploy.sh when the build succeeds
env \
  CONTAINER_NAME=build-env \
  OCAMLRUNPARAM=b \
  REPO_PACKAGE_NAME=xapi \
  REPO_CONFIGURE_CMD=./configure \
  REPO_BUILD_CMD=make \
  REPO_TEST_CMD='make test' \
  REPO_DOC_CMD='make doc-json' \
  bash travis-build-repo.sh && \
  ( ( test $TRAVIS_PULL_REQUEST = "false" && test $TRAVIS_BRANCH = "master" && bash deploy.sh ) || true )
