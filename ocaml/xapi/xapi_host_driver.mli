val create :
     __context:Context.t
  -> host:[`host] Ref.t
  -> name:string
  -> versions:string list
  -> active_version:string
  -> selected_version:string
  -> unit

val destroy : __context:Context.t -> self:[`Host_driver] Ref.t -> unit

val discover :
  __context:Context.t -> host:[`host] Ref.t -> (string * string list) list

val select :
  __context:Context.t -> self:[`Host_driver] Ref.t -> version:string -> unit

val deselect : __context:Context.t -> self:[`Host_driver] Ref.t -> unit
