Setting a label through an exclusive hold compiles:

  $ cat >exclusive.ml <<'EOF'
  > open Xapi_stdext_unix
  > module F (L : Lock_rw.S) = struct
  >   let f (h : Lock_rw.exclusive L.held) = L.set_label h 1
  > end
  > EOF
  $ ocamlfind ocamlc -package xapi-stdext-unix -c exclusive.ml

Setting it through a shared hold does not compile:

  $ sed 's/Lock_rw.exclusive/Lock_rw.shared/' exclusive.ml >shared.ml
  $ ocamlfind ocamlc -package xapi-stdext-unix -c shared.ml 2>&1 |
  > grep -c "These two variant types have no intersection"
  1
