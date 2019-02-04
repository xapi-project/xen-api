#!/bin/bash

set -ex

PACKAGE="xapi"
PINS="xapi:."
BASE_REMOTE="https://github.com/xapi-project/xs-opam.git"
DISTRO="debian-9-ocaml-4.07"
TRAVIS="$TRAVIS"
TRAVIS_JOB_ID="$TRAVIS_JOB_ID"

wget https://raw.githubusercontent.com/ocaml/ocaml-ci-scripts/master/.travis-docker.sh
. .travis-docker.sh

