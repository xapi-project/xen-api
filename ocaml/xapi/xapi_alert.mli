
(** Creates an alert/message and guarantees not to block. If called on the master it will use the
    internal Xapi_message.create function. If called on a slave it will enqueue the alert and 
    leave it for processing by a background thread. *)
val add : name:string -> priority:int64 -> cls:API.cls -> obj_uuid:string -> body:string -> unit

(** Calls the given function whenever values change *)
val edge_trigger : ('a -> 'a -> unit) -> 'a -> unit
