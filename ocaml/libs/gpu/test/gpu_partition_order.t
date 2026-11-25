Plans in the orders that are run compile:

  $ cat >ordered.ml <<'EOF'
  > open Gpu.Gpu_partition_order
  > let after_a_reboot (empty : (unit, unit, vfs_disabled * undivided) plan) =
  >   steps (divide () (vf_enable empty))
  > let release (empty : (unit, unit, vfs_enabled * divided) plan) :
  >     (unit, unit) ordered =
  >   steps (undivide () empty)
  > EOF
  $ ocamlfind ocamlc -package xapi-gpu -c ordered.ml

A division before the VFs are enabled does not compile:

  $ sed 's/(vf_enable empty)/empty/' ordered.ml >early.ml
  $ ocamlfind ocamlc -package xapi-gpu -c early.ml 2>err
  [2]
  $ grep -c "These two variant types have no intersection" err
  1

Steps written by hand are not the steps of a plan:

  $ sed 's/steps (undivide () empty)/[Undivide ()]/' ordered.ml >by_hand.ml
  $ ocamlfind ocamlc -package xapi-gpu -c by_hand.ml 2>err
  [2]
  $ grep -c "but an expression was expected of type" err
  1
