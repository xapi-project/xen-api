(** Represents local sessions, for use in emergency mode *)

type t = { 
  r: API.ref_session;
  pool: bool;
  last_active: Date.iso8601 }

val get_all: __context:Context.t -> API.ref_session list

val create: __context:Context.t -> pool:bool -> API.ref_session

val get_record: __context:Context.t -> self:API.ref_session -> t

val destroy: __context:Context.t -> self:API.ref_session -> unit

val local_session_hook: __context:Context.t -> session_id:API.ref_session -> bool
