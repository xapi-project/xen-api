val host : string -> string list -> string -> unit
(** [certify hostname alt_names path] creates (atomically) a PEM file at
[path] for hostname with alternative names [alt_names]. *)

module CLI : sig
  val main : unit -> unit
  (** [main] implements a command line interface which can be used to 
    build a stand-alone binary *)
end
