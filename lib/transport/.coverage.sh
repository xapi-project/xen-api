#!/bin/bash

set -ex

opam install bisect_ppx -y

make clean

BISECT_ENABLE=YES dune runtest --profile release
COVERALLS_URI=https://coveralls.io/api/v1/jobs

if [ -n "$TRAVIS" ]; then
  bisect-ppx-report \
    -I _build/default/ \
    -coveralls coverage.json \
    -service-name "travis-ci" \
    -service-job-id $TRAVIS_JOB_ID \
    `find . -name 'bisect*.out'`
  curl -L -F json_file=@./coverage.json $COVERALLS_URI
else
  echo "\$TRAVIS not set, so not uploading coverall report."
fi
