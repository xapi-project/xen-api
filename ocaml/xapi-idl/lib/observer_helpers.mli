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

(** ObserverAPI contains the declarations for the RPCs which are sent to
    Observer modules when the corresponding function is called on the Observer
    see ocaml/libs/tracing/ and ocaml/xapi/xapi_observer.ml *)
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
  (** [create dbg uuid name attributes endpoints enabled] notifies the
      forwarder that an Observer with [uuid] has been created. The subsequent
      parameters are the fields the Observer was created with. *)

  val destroy : (debug_info -> string -> (unit, Errors.error) R.comp) R.res
  (** [destroy dbg uuid] notifies the forwarder that an Observer with [uuid]
      has been destroyed. *)

  val set_enabled :
    (debug_info -> string -> bool -> (unit, Errors.error) R.comp) R.res
  (** [set_enabled dbg uuid enabled] notifies the fowarder that the Observer
      with [uuid] has had its enabled field set to [enabled]. *)

  val set_attributes :
    (   debug_info
     -> string
     -> (string * string) list
     -> (unit, Errors.error) R.comp
    )
    R.res
  (** [set_attributes dbg uuid attributes] notifies the fowarder that the
      Observer with [uuid] has had its attributes field set to [attributes]. *)

  val set_endpoints :
    (debug_info -> string -> string list -> (unit, Errors.error) R.comp) R.res
  (** [set_endpoints dbg uuid endpoints] notifies the fowarder that the Observer
      with [uuid] has had its endpoints field set to [endpoints]. *)

  val init : (debug_info -> (unit, Errors.error) R.comp) R.res
  (** [init dbg] notifies the forwarder that it should perform any tracing
      initialisation. *)

  val set_trace_log_dir :
    (debug_info -> string -> (unit, Errors.error) R.comp) R.res
  (** [set_trace_log_dir dbg dir] notifies the fowarder that the trace_log_dir
      has been set to [dir]. *)

  val set_export_interval :
    (debug_info -> float -> (unit, Errors.error) R.comp) R.res
  (** [set_export_interval dbg interval] notifies the fowarder that the interval
      between trace exports has been set to [interval]. *)

  val set_max_spans : (debug_info -> int -> (unit, Errors.error) R.comp) R.res
  (** [set_max_spans dbg spans] notifies the fowarder that the max number of
      spans has been set to [spans]. *)

  val set_max_traces : (debug_info -> int -> (unit, Errors.error) R.comp) R.res
  (** [set_max_traces dbg traces] notifies the fowarder that the max number of
      traces has been set to [traces]. *)

  val set_max_file_size :
    (debug_info -> int -> (unit, Errors.error) R.comp) R.res
  (** [set_max_file_size dbg file_size] notifies the fowarder that the max file
      size has been set to [file_size]. *)

  val set_host_id : (debug_info -> string -> (unit, Errors.error) R.comp) R.res
  (** [set_host_id dbg host_id] notifies the fowarder that the host to be traced
      has been set to [host_id]. *)

  val set_compress_tracing_files :
    (debug_info -> bool -> (unit, Errors.error) R.comp) R.res
  (** [set_compress_tracing_files dbg enabled] notifies the fowarder that the
      compression of tracing files has been set to [enabled]. *)
end

(** A Server_impl module will define how the Server responds to ObserverAPI calls *)
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

(** A Server for receiving ObserverAPI calls *)
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

(** A client for sending ObserverAPI calls to the above queue_name *)
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
