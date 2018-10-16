#!/bin/bash
# SUMMARY:
# Builds & tests xapi with coverage in a Ubuntu 16.04 Docker container with
# OCaml 4.04.2, then uploads the coverage information to coveralls.

set -ex

# Currently there is no way of specifying OPAM depexts for multiple versions of
# a given disto, and our current depexts only work with Ubuntu >= 16.04, due to
# a change in packages (libsystemd-dev). Since the build environments of Travis
# are older then Ubuntu 16.04, we have to run the build in a Docker container
# with an appropriate Ubuntu version.
# We need to pass some Travis environment variables to the container to enable
# uploading to coveralls and detection of Travis CI.
docker run --rm --volume=$PWD:/mnt --workdir=/mnt \
  --env "TRAVIS=$TRAVIS" \
  --env "TRAVIS_JOB_ID=$TRAVIS_JOB_ID" \
  ocaml/opam2:ubuntu-18.04-ocaml-4.06 \
  bash -ex -c '
sudo apt-get update

sudo chown -R $(whoami) .

# replace the base remote with xs-opam
opam repository remove default
opam repository add xs-opam https://github.com/xapi-project/xs-opam.git

# install the dependencies of xapi
sudo apt install -y libpci-dev
ls *.opam | xargs -n1 basename -s.opam | xargs -n1 -I '{}' opam pin add --no-action '{}' .
opam depext --yes xapi
opam install --deps-only xapi
'

