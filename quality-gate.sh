#!/bin/bash

set -e

list-hd () {
  N=329
  LIST_HD=$(git grep -r --count 'List.hd' -- **/*.ml | cut -d ':' -f 2 | paste -sd+ - | bc)
  if [ "$LIST_HD" -eq "$N" ]; then
    echo "OK counted $LIST_HD usages"
  else
    echo "ERROR expected $N List.hd usages, got $LIST_HD" 1>&2
    exit 1
  fi
}

verify-cert () {
  N=13
  NONE=$(git grep -r --count 'verify_cert:None' -- **/*.ml | cut -d ':' -f 2 | paste -sd+ - | bc)
  if [ "$NONE" -eq "$N" ]; then
    echo "OK counted $NONE usages of verify_cert:None"
  else
    echo "ERROR expected $N verify_cert:None usages, got $NONE" 1>&2
    exit 1
  fi
}

mli-files () {
  N=515
  # do not count ml files from the tests in ocaml/{tests/perftest/quicktest}
  MLIS=$(git ls-files -- '**/*.mli' | grep -vE "ocaml/tests|ocaml/perftest|ocaml/quicktest" | xargs -I {} sh -c "echo {} | cut -f 1 -d '.'" \;)
  MLS=$(git  ls-files -- '**/*.ml'  | grep -vE "ocaml/tests|ocaml/perftest|ocaml/quicktest" | xargs -I {} sh -c "echo {} | cut -f 1 -d '.'" \;)
  num_mls_without_mlis=$(comm -23 <(sort <<<"$MLS") <(sort <<<"$MLIS") | wc -l)
  if [ "$num_mls_without_mlis" -eq "$N" ]; then
    echo "OK counted $num_mls_without_mlis .ml files without an .mli"
  else
    echo "ERROR expected $N .ml files without .mlis, got $num_mls_without_mlis."\
         "If you created some .ml files, they are probably missing corresponding .mli's" 1>&2
    exit 1
  fi
}

structural-equality () {
  N=7
  EQ=$(git grep -r --count ' == ' -- '**/*.ml' ':!ocaml/sdk-gen/**/*.ml' | cut -d ':' -f 2 | paste -sd+ - | bc)
  if [ "$EQ" -eq "$N" ]; then
    echo "OK counted $EQ usages of ' == '"
  else
    echo "ERROR expected $N usages of ' == ', got $EQ; use = rather than ==" 1>&2
    exit 1
  fi

  if git grep -r --count ' != ' -- '**/*.ml' ':!ocaml/sdk-gen/**/*.ml'; then
    echo "ERROR expected no usages of ' != '; use <> rather than !=" 1>&2
    exit 1
  else
    echo "OK found no usages of ' != '"
  fi
}

list-hd
verify-cert
mli-files
structural-equality
