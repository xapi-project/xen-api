(** This module extends {{!Base.Char}[Base.Char]}, adding [Identifiable] for making char
    identifiers and [Quickcheckable] to facilitate automated testing with pseudorandom
    data.
*)

type t = char [@@deriving typerep]

(** {2 The signature included from [Base.Char]} *)

include module type of struct include Base.Char end
  with type t := t (** @open *)

(** {2 Extensions} *)

include Identifiable.S
  with type t := t
   and type comparator_witness := comparator_witness

include Quickcheckable.S with type t := t

val gen_digit      : t Quickcheck.Generator.t
val gen_lowercase  : t Quickcheck.Generator.t
val gen_uppercase  : t Quickcheck.Generator.t
val gen_alpha      : t Quickcheck.Generator.t
val gen_alphanum   : t Quickcheck.Generator.t
val gen_print      : t Quickcheck.Generator.t
val gen_whitespace : t Quickcheck.Generator.t
