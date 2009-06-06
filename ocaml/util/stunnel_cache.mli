
(** Connects via stunnel (optionally via an external 'close fds' wrapper) to
    a host and port. If there is a suitable stunnel in the cache then this 
    will be used, otherwise we make a fresh one. *)
val connect :
  ?use_external_fd_wrapper:bool ->
  ?write_to_log:(string -> unit) -> string -> int -> Stunnel.t

(** Adds a reusable stunnel to the cache *)
val add : Stunnel.t -> unit

(** Given a host and port return a cached stunnel, or throw Not_found *)
val remove : string -> int -> Stunnel.t

(** Empty the cache of all stunnels *)
val flush : unit -> unit

(** GCs old stunnels *)
val gc : unit -> unit
