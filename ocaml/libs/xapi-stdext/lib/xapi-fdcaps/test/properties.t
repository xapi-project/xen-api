Check that we get compile errors when trying to use a read-only or write-only property with the opposite operation:

  $ cat >t.ml <<'EOF'
  > open Xapi_fdcaps.Properties
  > let _ = as_readable (make `wronly `reg)
  > EOF
  $ ocamlfind ocamlc -package xapi-stdext-unix.fdcaps -c t.ml 2>&1 |
  > grep -c "The second variant type does not allow tag(s)"
  1

  $ cat >t.ml <<'EOF'
  > open Xapi_fdcaps.Properties
  > let _ = as_writable (make `rdonly `reg)
  > EOF
  $ ocamlfind ocamlc -package xapi-stdext-unix.fdcaps -c t.ml 2>&1 |
  > grep -c "The second variant type does not allow tag(s)"
  1
