val queue_name : string

val default_path : string

module Errors : sig
  type error =
    | Internal_error of string
    | Unimplemented of string
    | Unknown_error

  val typ_of_error : error Rpc.Types.typ

  val error : error Rpc.Types.def
end

exception Observer_error of Errors.error

type debug_info = string

module ObserverAPI : functor (R : Idl.RPC) -> sig
  val description : Idl.Interface.description

  val implementation : R.implementation

  val create :
    (   debug_info
     -> string
     -> string
     -> (string * string) list
     -> string list
     -> bool
     -> (unit, Errors.error) R.comp
    )
    R.res

  val destroy : (debug_info -> string -> (unit, Errors.error) R.comp) R.res

  val set_enabled :
    (debug_info -> string -> bool -> (unit, Errors.error) R.comp) R.res

  val set_attributes :
    (   debug_info
     -> string
     -> (string * string) list
     -> (unit, Errors.error) R.comp
    )
    R.res

  val set_endpoints :
    (debug_info -> string -> string list -> (unit, Errors.error) R.comp) R.res

  val init : (debug_info -> (unit, Errors.error) R.comp) R.res

  val set_trace_log_dir :
    (debug_info -> string -> (unit, Errors.error) R.comp) R.res

  val set_export_interval :
    (debug_info -> float -> (unit, Errors.error) R.comp) R.res

  val set_max_spans : (debug_info -> int -> (unit, Errors.error) R.comp) R.res

  val set_max_traces : (debug_info -> int -> (unit, Errors.error) R.comp) R.res

  val set_max_file_size :
    (debug_info -> int -> (unit, Errors.error) R.comp) R.res

  val set_host_id : (debug_info -> string -> (unit, Errors.error) R.comp) R.res

  val set_compress_tracing_files :
    (debug_info -> bool -> (unit, Errors.error) R.comp) R.res
end

module type Server_impl = sig
  type context = unit

  val create :
       context
    -> dbg:debug_info
    -> uuid:string
    -> name_label:string
    -> attributes:(string * string) list
    -> endpoints:string list
    -> enabled:bool
    -> unit

  val destroy : context -> dbg:debug_info -> uuid:string -> unit

  val set_enabled :
    context -> dbg:debug_info -> uuid:string -> enabled:bool -> unit

  val set_attributes :
       context
    -> dbg:debug_info
    -> uuid:string
    -> attributes:(string * string) list
    -> unit

  val set_endpoints :
    context -> dbg:debug_info -> uuid:string -> endpoints:string list -> unit

  val init : context -> dbg:debug_info -> unit

  val set_trace_log_dir : context -> dbg:debug_info -> dir:string -> unit

  val set_export_interval : context -> dbg:debug_info -> interval:float -> unit

  val set_max_spans : context -> dbg:debug_info -> spans:int -> unit

  val set_max_traces : context -> dbg:debug_info -> traces:int -> unit

  val set_max_file_size : context -> dbg:debug_info -> file_size:int -> unit

  val set_host_id : context -> dbg:debug_info -> host_id:string -> unit

  val set_compress_tracing_files :
    context -> dbg:debug_info -> enabled:bool -> unit
end

module Server : functor (_ : Server_impl) () -> sig
  module S : sig
    val create :
         (   debug_info
          -> string
          -> string
          -> (string * string) list
          -> string list
          -> bool
          -> unit
         )
      -> unit

    val destroy : (debug_info -> string -> unit) -> unit

    val set_enabled : (debug_info -> string -> bool -> unit) -> unit

    val set_attributes :
      (debug_info -> string -> (string * string) list -> unit) -> unit

    val set_endpoints : (debug_info -> string -> string list -> unit) -> unit

    val init : (debug_info -> unit) -> unit

    val set_trace_log_dir : (debug_info -> string -> unit) -> unit

    val set_export_interval : (debug_info -> float -> unit) -> unit

    val set_max_spans : (debug_info -> int -> unit) -> unit

    val set_max_traces : (debug_info -> int -> unit) -> unit

    val set_max_file_size : (debug_info -> int -> unit) -> unit

    val set_host_id : (debug_info -> string -> unit) -> unit

    val set_compress_tracing_files : (debug_info -> bool -> unit) -> unit
  end

  val process : Rpc.call -> Rpc.response
end

module Client : sig
  val create :
       debug_info
    -> string
    -> string
    -> (string * string) list
    -> string list
    -> bool
    -> unit

  val destroy : debug_info -> string -> unit

  val set_enabled : debug_info -> string -> bool -> unit

  val set_attributes : debug_info -> string -> (string * string) list -> unit

  val set_endpoints : debug_info -> string -> string list -> unit

  val init : debug_info -> unit

  val set_trace_log_dir : debug_info -> string -> unit

  val set_export_interval : debug_info -> float -> unit

  val set_max_spans : debug_info -> int -> unit

  val set_max_traces : debug_info -> int -> unit

  val set_max_file_size : debug_info -> int -> unit

  val set_host_id : debug_info -> string -> unit

  val set_compress_tracing_files : debug_info -> bool -> unit
end
