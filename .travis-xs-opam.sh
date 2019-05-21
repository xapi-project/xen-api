#!/bin/bash

set -ex

PACKAGE="xapi"
PINS="xapi:."
BASE_REMOTE="https://github.com/xapi-project/xs-opam.git"
BASE_REMOTE_BRANCH="feature/REQ-720/master"
DISTRO="debian-unstable"
OCAML_VERSION="4.07"

wget https://raw.githubusercontent.com/ocaml/ocaml-ci-scripts/master/.travis-docker.sh
. .travis-docker.sh

