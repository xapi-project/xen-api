#! /bin/sh

echo "----- Example 1: filtering -----"
ocamlfind ocamlopt -o filtering filtering.ml -package yojson -linkpkg
./filtering < filtering.json
