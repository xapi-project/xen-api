(* Functions for running ocamltest tests as part of quicktest. *)

(** Runs the given test as part of quicktest, using the *)
(** quicktest printer to provide pretty-printed output. *)
val run_from_within_quicktest : Ocamltest.test -> unit
