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
  ocaml/opam:ubuntu-16.04_ocaml-4.04.2 \
  bash -ex -c '
sudo apt-get update

sudo chown -R $(whoami) .

# replace the base remote with xs-opam
opam repository remove default
opam repository add xs-opam https://github.com/xapi-project/xs-opam.git

# install the dependencies of xapi
sudo apt install -y libpci-dev
opam pin add --no-action xapi .
opam depext --yes xapi
opam install --deps-only xapi

# build and test xapi with coverage, then submit the coverage information to coveralls

opam install -y bisect_ppx ocveralls

export BISECT_ENABLE=YES

ulimit -s 16384

# the test fails when run on debian-stable (it passed on Ubuntu though)
# disable it when running coverage
# -p does not work to filter out subdirectories though
# because the test in pci does not say which package it is part of
rm -rf ocaml/pci/lib_test
jbuilder runtest

declare -a outs
outs=($(find . | grep bisect.*.out))
bisect-ppx-report -I $(dirname ${outs[1]}) -text report ${outs[@]}
bisect-ppx-report -I $(dirname ${outs[1]}) -summary-only -text summary ${outs[@]}
if [ -n "$HTML" ]; then bisect-ppx-report -I $(dirname ${outs[1]}) -html ../html-report ${outs[@]}; fi

if [ -n "$TRAVIS" ]; then
  echo "\$TRAVIS set; running ocveralls and sending to coveralls.io..."
  ocveralls --prefix _build/default ${outs[@]} --send
else
  echo "\$TRAVIS not set; displaying results of bisect-report..."
  cat report
  cat summary
fi
'

