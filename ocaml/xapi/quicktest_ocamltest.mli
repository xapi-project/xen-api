(* Functions for running ocamltest tests as part of quicktest. *)
(* Author: Jonathan Knowles                                    *)
(* Copyright: 2008 Citrix Systems Research & Development Ltd.  *)

(** Runs the given test as part of quicktest, using the *)
(** quicktest printer to provide pretty-printed output. *)
val run_from_within_quicktest : Ocamltest.test -> unit
