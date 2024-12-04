module D : Debug.DEBUG

module Unixext = Xapi_stdext_unix.Unixext
module T = Xapi_host_driver_tool

val with_lock : Mutex.t -> (unit -> 'a) -> 'a

val selection_mutex : Mutex.t

val ( // ) : string -> string -> string

val invalid_value : string -> string -> 'a

val internal_error : ('a, unit, string, 'b) format4 -> 'a

val drivertool : string list -> string

module Variant : sig
  val create :
       __context:Context.t
    -> name:string
    -> version:string
    -> driver:[`Host_driver] API.Ref.t
    -> hw_present:bool
    -> priority:float
    -> dev_status:string
    -> [`Driver_variant] Ref.t

  val destroy : __context:Context.t -> self:[`Driver_variant] API.Ref.t -> unit

  val select : __context:Context.t -> self:[`Driver_variant] API.Ref.t -> unit
end

val create :
     __context:Context.t
  -> host:[`host] API.Ref.t
  -> name:string
  -> friendly_name:string
  -> _type:string
  -> description:string
  -> info:string
  -> active_variant:[`Driver_variant] API.Ref.t
  -> selected_variant:[`Driver_variant] API.Ref.t
  -> [`Host_driver] Ref.t

val destroy : __context:Context.t -> self:[`Host_driver] API.Ref.t -> unit

val select :
     __context:Context.t
  -> self:[`Host_driver] API.Ref.t
  -> variant:[`Driver_variant] API.Ref.t
  -> unit

val deselect : __context:Context.t -> self:[`Host_driver] API.Ref.t -> unit

val reset : __context:Context.t -> host:[< Ref.all] Ref.t -> unit

val scan : __context:Context.t -> host:[`host] API.Ref.t -> unit

val rescan : __context:Context.t -> host:[`host] API.Ref.t -> unit
