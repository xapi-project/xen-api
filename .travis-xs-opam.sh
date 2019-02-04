#!/bin/bash

set -ex

PACKAGE="xapi"
PINS="xapi:."
BASE_REMOTE="https://github.com/xapi-project/xs-opam.git"
DISTRO="debian-9-ocaml-4.07"

wget https://raw.githubusercontent.com/ocaml/ocaml-ci-scripts/master/.travis-docker.sh
. .travis-docker.sh

