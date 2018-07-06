(** Utility functions for marshalling to and from {!Bigstring}.

    In all functions below, [pos] is the index into the bigstring to read from or write to
    and an exception is raised if that index is invalid. The default is 0.
*)

open! Import

(** Marshals value [_] to the bigstring at most [len] bytes. *)
val marshal_blit
  : ?flags : Marshal.extern_flags list  (** default = [] *)
  -> _
  -> ?pos : int
  -> ?len : int                         (** default = length buf - pos *)
  -> Bigstring.t
  -> int

(** Marshals value [_] to a new bigstring. This function may need two times more memory
    than [marshal_blit].

    @param flags default = [[]]
*)
val marshal
  : ?flags : Marshal.extern_flags list
  -> _
  -> Bigstring.t

(** The length of marshalled data in the bigstring *)
val marshal_data_size : ?pos : int -> Bigstring.t -> int

(** Unmarshals a value from the bigstring and/or returns the index of the byte in the
    bigstring right after the unmarshalled value. *)
val unmarshal      : ?pos : int -> Bigstring.t -> _
val unmarshal_next : ?pos : int -> Bigstring.t -> _ * int
val skip           : ?pos : int -> Bigstring.t ->     int
