val host : string -> string list -> string -> unit
(** [certify hostname alt_names path] creates (atomically) a PEM file at
[path] for hostname with alternative names [alt_names]. *)
