#!/bin/bash

set -e

list-hd () {
  LIST_HD=$(git grep -r --count 'List.hd' -- **/*.ml | cut -d ':' -f 2 | paste -sd+ - | bc)
  if [ "$LIST_HD" -eq 296 ]; then
    echo "OK counted $LIST_HD usages"
  else
    echo "ERROR expected 296 List.hd usages, got $LIST_HD" 1>&2
    exit 1
  fi
}

verify-cert () {
  NONE=$(git grep -r --count 'verify_cert:None' -- **/*.ml | cut -d ':' -f 2 | paste -sd+ - | bc)
  if [ "$NONE" -eq 7 ]; then
    echo "OK counted $NONE usages of verify_cert:None"
  else
    echo "ERROR expected 7 verify_cert:None usages, got $NONE" 1>&2
    exit 1
  fi
}

mli-files () {
  MLIS=$(git ls-files -- '**/*.mli' | xargs -I {} sh -c "echo {} | cut -f 1 -d '.'" \;)
  MLS=$(git  ls-files -- '**/*.ml'  | xargs -I {} sh -c "echo {} | cut -f 1 -d '.'" \;)
  num_mls_without_mlis=$(comm -23 <(sort <<<"$MLS") <(sort <<<"$MLIS") | wc -l)
  if [ "$num_mls_without_mlis" -eq 393 ]; then
    echo "OK counted $num_mls_without_mlis .ml files without an .mli"
  else
    echo "ERROR expected 393 .ml files without .mlis, got $num_mls_without_mlis."\
         "If you created some .ml files, they are probably missing corresponding .mli's" 1>&2
    exit 1
  fi
}

list-hd
verify-cert
mli-files
