
type t = 
  | Master  
  | Slave of string (** IP address *)
  | Broken

(** Returns the role of this node *)
val get_role: unit -> t
(** Reset the role on disk, takes effect on next server restart only! *)
val set_role: t -> unit

(** Returns true if this node is a master *)
val is_master: unit -> bool
(** Returns true if this node is a slave *)
val is_slave: unit -> bool
(** Returns true if this node is broken *)
val is_broken: unit -> bool

exception This_host_is_a_master
exception This_host_is_broken

(** If this node is a slave, returns the IP address of its master *)
val get_master_address: unit -> string
