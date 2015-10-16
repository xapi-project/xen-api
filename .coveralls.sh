#!/bin/sh
set -e
# Make sure we're not echoing any sensitive data
set +x

COVERAGE_DIR=.coverage
rm -rf $COVERAGE_DIR
mkdir -p $COVERAGE_DIR
pushd $COVERAGE_DIR
if [ -z "$KEEP" ]; then trap "popd; rm -rf $COVERAGE_DIR" EXIT; fi

$(which cp) -r ../* .

eval `opam config env`
opam pin add -n bisect_ppx git://github.com/rleonid/bisect_ppx#0.2.2
opam install -y bisect_ppx oasis ocveralls

sed -i '/BuildDepends:/ s/$/, bisect_ppx/' _oasis
if [ -f ../.coverage.excludes ]; then
  ln -s ../.coverage.excludes
  sed -i '/ByteOpt:/ s/$/ -ppxopt bisect_ppx,"-exclude-file ..\/.coverage.excludes"/' _oasis
  sed -i '/NativeOpt:/ s/$/ -ppxopt bisect_ppx,"-exclude-file ..\/.coverage.excludes"/' _oasis
fi
oasis setup

./configure
make

find . -name bisect* | xargs rm -f
./basic-rpc-test.sh -runner sequential

bisect-report bisect*.out -I _build -text report
bisect-report bisect*.out -I _build -summary-only -text summary
(cd _build; bisect-report ../bisect*.out -html ../report-html)

if [ -n "$TRAVIS" ]; then
  echo "\$TRAVIS set; running ocveralls and sending to coveralls.io..."
  ocveralls --prefix _build bisect*.out --send
else
  echo "\$TRAVIS not set; displaying results of bisect-report..."
  cat report
  cat summary
fi
