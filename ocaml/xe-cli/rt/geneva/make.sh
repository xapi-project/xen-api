#!/bin/sh
PATH=/mingw/bin:/ocaml/bin:$PATH
OCAMLC=ocamlopt.exe
OCAMLLIB=c:\\SFU\\ocaml\\lib
export OCAMLLIB
export PATH
$OCAMLC -c utils.ml
$OCAMLC -c cli_utils.ml
$OCAMLC -c cli_test.ml
$OCAMLC -o cli_test.exe unix.cmxa utils.cmx cli_utils.cmx cli_test.cmx
