module Uuidm : sig
  type t = Uuidm.t

  include module type of Uuidm with type t := t

  val typ_of : t Rpc.Types.typ

  val t_of_sexp : Sexplib.Sexp.t -> t

  val sexp_of_t : t -> Sexplib.Sexp.t
end
