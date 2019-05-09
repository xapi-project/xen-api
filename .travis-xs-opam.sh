#!/bin/bash

set -ex

PACKAGE="xapi"
PINS="xapi:. xapi-cli-protocol:. xapi-client:. xapi-consts:. xapi-database:. xapi-datamodel:. xapi-types:. xe:."
BASE_REMOTE="https://github.com/xapi-project/xs-opam.git"
DISTRO="debian-unstable"
OCAML_VERSION="4.07"

wget https://raw.githubusercontent.com/ocaml/ocaml-ci-scripts/master/.travis-docker.sh
. .travis-docker.sh

