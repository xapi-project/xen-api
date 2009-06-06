
(** Common definitions used across the ocaml code *)
module O = Ocaml_syntax
module DT = Datamodel_types
module DU = Datamodel_utils
module DM = Datamodel
module OU = Ocaml_utils

(* XXX: probably should move stuff out of Gen_client and put it here instead *)

let context = "__context"
let context_arg = O.Named(context, "Context.t") 

