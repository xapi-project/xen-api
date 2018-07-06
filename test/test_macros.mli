open Sexplib

module type Load = sig
  val load_sexp_conv_exn : string -> (Sexp.t -> 'a) -> 'a
  val load_sexps_conv    : string -> (Sexp.t -> 'a) -> 'a Sexp.Annotated.conv list
end

(** [make (module Load)] runs a bunch of tests on [Load] functions and prints the output.
    If [reference] is supplied, [Load]'s output is compared against [reference]'s output,
    and the output is only printed if they differ. *)
val make
  :  ?reference:(module Load)
  -> (module Load)
  -> unit

