#!/bin/bash

set -e

list-hd () {
  N=318
  LIST_HD=$(git grep -r --count 'List.hd' -- **/*.ml | cut -d ':' -f 2 | paste -sd+ - | bc)
  if [ "$LIST_HD" -eq "$N" ]; then
    echo "OK counted $LIST_HD List.hd usages"
  else
    echo "ERROR expected $N List.hd usages, got $LIST_HD" 1>&2
    exit 1
  fi
}

verify-cert () {
  N=14
  NONE=$(git grep -r --count 'verify_cert:None' -- **/*.ml | cut -d ':' -f 2 | paste -sd+ - | bc)
  if [ "$NONE" -eq "$N" ]; then
    echo "OK counted $NONE usages of verify_cert:None"
  else
    echo "ERROR expected $N verify_cert:None usages, got $NONE" 1>&2
    exit 1
  fi
}

mli-files () {
  N=522
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
  N=10
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

vtpm-unimplemented () {
  N=2
  VTPM=$(git grep -r --count 'maybe_raise_vtpm_unimplemented' -- **/*.ml | cut -d ':' -f 2 | paste -sd+ - | bc)
  if [ "$VTPM" -eq "$N" ]; then
    echo "OK found $VTPM usages of vtpm unimplemented errors"
  else
    echo "ERROR expected $N usages of unimplemented vtpm functionality, got $VTPM." 1>&2
    exit 1
  fi
}

vtpm-fields () {
  A=$(git grep -hc "vTPM'_.*:" ocaml/xapi/importexport.ml)
  B=$(git grep -hc ' field' ocaml/idl/datamodel_vtpm.ml)
  case "$A/$B" in
    5/6)
      echo "OK found $A/$B VTPM fields in importexport.ml datamodel_vtpm.ml"
      ;;
    *)
      echo "ERROR have VTPM fields changed? $A/$B - check importexport.ml" 1>&2
      exit 1
      ;;
  esac
}

list-hd
verify-cert
mli-files
structural-equality
vtpm-unimplemented
vtpm-fields

