type t

val make : Xc.domid -> nativeint -> int -> Mmap.mmap_interface -> Event.t -> t
val close : t -> unit

val get_path : t -> string
val get_id : t -> Xc.domid
val get_interface : t -> Mmap.mmap_interface
val get_mfn : t -> nativeint
val get_remote_port : t -> int

val dump : t -> out_channel -> unit

val notify : t -> unit
val bind_interdomain : t -> unit
val is_dom0 : t -> bool
