
(** Call out to the script to bring up a PIF on this host. The script will be skipped if
    PIF.currently_attached is still marked as true UNLESS management_interface is set. *)
val bring_pif_up : __context:Context.t -> ?management_interface:bool -> API.ref_PIF -> unit

(** Call out to the script to take down a PIF on this host *)
val bring_pif_down : __context:Context.t -> API.ref_PIF -> unit

val with_local_lock : (unit -> 'a) -> 'a
