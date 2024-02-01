type t

val to_string : ?upper:bool -> t -> string

module Hash : sig
  (** hash a string (deterministically) into a UUID. This uses
      namespace UUID e93e0639-2bdb-4a59-8b46-352b3f408c19. *)

  (* UUID Version 5 derived from argument string and namespace UUID *)
  val string : string -> t
end
