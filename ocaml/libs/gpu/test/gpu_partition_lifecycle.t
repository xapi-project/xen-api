A match over every transition of a VM's lifecycle compiles with warning 8
as an error:

  $ cat >total.ml <<'EOF'
  > open Gpu.Gpu_partition_lifecycle
  > let f = function
  >   | Create | Reserve | Confirm | Attach | Release_halted
  >   | Release_suspended | Checkpoint | Abort | Restart_sweep -> ()
  > EOF
  $ ocamlfind ocamlc -package xapi-gpu -w @8 -c total.ml

The same match without one transition is refused when warning 8 is an error:

  $ sed 's/ | Restart_sweep//' total.ml >partial.ml
  $ ocamlfind ocamlc -package xapi-gpu -w @8 -c partial.ml 2>err
  [2]
  $ grep -c "this pattern-matching is not exhaustive" err
  1
