(** This module provides dummy implementations for each Observer function.
    These are intended to be used to fill in the functions that the module will
    not ever use, as they will raise an Unimplemented error if called *)
module Observer : sig
  type context = unit

  val create :
       context
    -> dbg:string
    -> uuid:string
    -> name_label:string
    -> attributes:(string * string) list
    -> endpoints:string list
    -> enabled:bool
    -> unit

  val destroy : context -> dbg:string -> uuid:string -> unit

  val set_enabled : context -> dbg:string -> uuid:string -> enabled:bool -> unit

  val set_attributes :
       context
    -> dbg:string
    -> uuid:string
    -> attributes:(string * string) list
    -> unit

  val set_endpoints :
    context -> dbg:string -> uuid:string -> endpoints:string list -> unit

  val init : context -> dbg:string -> unit

  val set_trace_log_dir : context -> dbg:string -> dir:string -> unit

  val set_export_interval : context -> dbg:string -> interval:float -> unit

  val set_max_spans : context -> dbg:string -> spans:int -> unit

  val set_max_traces : context -> dbg:string -> traces:int -> unit

  val set_max_file_size : context -> dbg:string -> file_size:int -> unit

  val set_host_id : context -> dbg:string -> host_id:string -> unit

  val set_compress_tracing_files : context -> dbg:string -> enabled:bool -> unit
end
