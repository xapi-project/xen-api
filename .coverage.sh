#!/bin/sh

set -ex

COVERAGE_DIR=.coverage
rm -rf $COVERAGE_DIR
mkdir -p $COVERAGE_DIR
pushd $COVERAGE_DIR
if [ -z "$KEEP" ]; then trap "popd; rm -rf $COVERAGE_DIR" EXIT; fi

# copy over everything
$(which cp) -r ../* .

# prepare the environment
eval `opam config env`
opam install bisect_ppx ocveralls -y

# run the tests with coverage enabled
export BISECT_ENABLE=YES
jbuilder runtest

# move all bisect files here
find . -name 'bisect*.out' -exec mv {} . \;

DIRECTORIES=$(find _build/default -type d -not -path '*/\.*')
printf -v INCLUDES " -I %s" $DIRECTORIES

bisect-ppx-report bisect*.out $INCLUDES -text report
bisect-ppx-report bisect*.out $INCLUDES -summary-only -text summary
bisect-ppx-report bisect*.out $INCLUDES -html report-html

if [ -n "$TRAVIS" ]; then
  echo "\$TRAVIS set; running ocveralls and sending to coveralls.io..."
  ocveralls --prefix _build/default bisect*.out --send
else
  echo "\$TRAVIS not set; displaying results of bisect-ppx-report..."
  cat report
  cat summary
fi

