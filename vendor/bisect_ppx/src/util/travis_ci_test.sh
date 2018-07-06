#!/usr/bin/env bash

set -e
set -x

travis_install_on_linux () {
    # Install OCaml and OPAM PPA
    sudo add-apt-repository -y ppa:avsm/ocaml42+opam12
    sudo apt-get update -qq

    sudo apt-get install -qq opam time git

    case "$OCAML_VERSION" in
        4.02)
            sudo apt-get install -qq ocaml-nox camlp4-extra
            opam init -y ;;
        4.03)
            opam init -y --compiler=4.03.0 ;;
        4.04)
            opam init -y --compiler=4.04.2 ;;
        4.05)
            opam init -y --compiler=4.05.0 ;;
        4.06)
            opam init -y --compiler=4.06.1 ;;
        4.07)
            opam init -y --compiler=4.07.0+beta2 ;;
        *)
            echo Unknown $OCAML_VERSION
            exit 1 ;;
    esac
}

travis_install_on_osx () {
    brew update > /dev/null
    # See https://github.com/Homebrew/homebrew-core/issues/26358.
    brew upgrade python > /dev/null
    brew install opam

    case "$OCAML_VERSION" in
        4.02)
            opam init -y --compiler=4.02.3 ;;
        4.03)
            opam init -y --compiler=4.03.0 ;;
        4.04)
            opam init -y --compiler=4.04.2 ;;
        4.05)
            opam init -y --compiler=4.05.0 ;;
        4.06)
            opam init -y --compiler=4.06.1 ;;
        *)
            echo Unknown $OCAML_VERSION
            exit 1 ;;
    esac
}

case $TRAVIS_OS_NAME in
    osx) travis_install_on_osx ;;
    linux) travis_install_on_linux ;;
    *) echo "Unknown $TRAVIS_OS_NAME"; exit 1
esac

# Prepare environment
eval `opam config env`

# Check packages
ocaml -version | grep $OCAML_VERSION
opam --version
git --version

echo
echo "Install dependencies"
echo
opam install -y ocamlfind ocamlbuild ocaml-migrate-parsetree ppx_tools_versioned

echo
echo "Compiling"
echo
make build

opam install -y ounit
# opam install -y ppx_blob ppx_deriving

echo
echo "Testing"
echo
make test

echo
echo "Testing package usage and Ocamlbuild plugin"
echo
make usage

echo
echo "Checking OPAM file"
echo
ls *.opam | xargs -L1 opam lint

echo
echo "Testing installation"
echo
make clean
opam pin add -yn bisect_ppx .
opam install -y bisect_ppx
ocamlfind query bisect_ppx bisect_ppx.runtime
which bisect-ppx-report
opam pin add -yn bisect_ppx-ocamlbuild .
opam install -y bisect_ppx-ocamlbuild
ocamlfind query bisect_ppx-ocamlbuild

# Currently unused; awaiting restoration of self-instrumentation.
if [ "$COVERALLS" = yes ]
then
    echo
    echo "Submitting coverage report"
    echo
    opam install -y ocveralls
    make dev tests
    make -C tests coverage
    ocveralls --prefix _build.instrumented tests/_coverage/meta*.out --send
fi
