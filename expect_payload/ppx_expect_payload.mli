open Ppxlib
open Expect_test_common.Std

(** Translate a compile time location to a runtime location *)
val transl_loc : Location.t -> File.Location.t

val make
  :  is_exact:bool
  -> (Location.t * string * string option) (* string loc, string, tag *) option
  -> extension_id_loc:Location.t
  -> Expectation.Raw.t

val pattern
  : unit
  -> (Parsetree.payload,
      (Location.t * string * string option) option -> 'a,
      'a) Ast_pattern.t
