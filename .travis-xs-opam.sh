#!/bin/bash

set -ex

PACKAGE="xapi"
PINS="xapi:. xapi-cli-protocol:. xapi-client:. xapi-consts:. xapi-database:. xapi-datamodel:. xapi-types:. xe:."
BASE_REMOTE_BRANCH="feature/REQ-453/master"

wget https://raw.githubusercontent.com/ocaml/ocaml-ci-scripts/master/.travis-docker.sh
wget https://raw.githubusercontent.com/xapi-project/xs-opam/master/tools/xs-opam-ci.env
. xs-opam-ci.env
. .travis-docker.sh

