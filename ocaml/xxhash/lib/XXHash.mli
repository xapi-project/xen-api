(** xxHash - Extremely fast hash algorithm *)

exception XXH_error

module type XXHASH = sig
  type hash
  type state

  (** {2 Short input} *)

  (** [hash ~seed input] returns the hash of [input]. This is equivalent to
    {[
      let state = create () in
      reset ~seed state;
      update state input;
      let hash = digest state in
      free state;
      hash
    ]}
  *)
  val hash : ?seed:hash -> string -> hash

  (** {2 Streaming} *)

  (** [reset ~seed state] resets [state] with an optional seed. *)
  val reset : ?seed:hash -> state -> unit

  (** [update state input] adds [input] to [state]. *)
  val update : state -> string -> unit

  (** [digest state] returns the hash all input added to [state]. *)
  val digest : state -> hash

  (** [with_state ~seed f] returns the hash of all input added to [state] by [f].
      [with_state (fun state -> update state input)] is equivalent to [hash input]. *)
  val with_state : ?seed:hash -> (state -> unit) -> hash
end

module XXH32 : (XXHASH with type hash = nativeint)
module XXH64 : (XXHASH with type hash = int64)
