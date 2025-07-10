(*
 * Copyright (c) Cloud Software Group
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

open Rpc
open Idl

module D = Debug.Make (struct let name = "observer_interface" end)

open D

let service_name = "observer"

let queue_name = Xcp_service.common_prefix ^ service_name

let default_sockets_dir = "/var/lib/xcp"

let default_path = Filename.concat default_sockets_dir service_name

let uri () = "file:" ^ default_path

module Errors = struct
  type error =
    | Internal_error of string
    | Unimplemented of string
    | Unknown_error
  [@@default Unknown_error] [@@deriving rpcty]
end

exception Observer_error of Errors.error

let err =
  let open Error in
  {
    def= Errors.error
  ; raiser=
      (fun e ->
        let exn = Observer_error e in
        error "%s (%s)" (Printexc.to_string exn) __LOC__ ;
        raise exn
      )
  ; matcher=
      (function
      | Observer_error e as exn ->
          error "%s (%s)" (Printexc.to_string exn) __LOC__ ;
          Some e
      | exn ->
          error "%s (%s)" (Printexc.to_string exn) __LOC__ ;
          Some (Internal_error (Printexc.to_string exn))
      )
  }

(** An uninterpreted string associated with the operation. *)
type debug_info = string [@@deriving rpcty]

module ObserverAPI (R : RPC) = struct
  open R
  open TypeCombinators

  let description =
    let open Interface in
    {
      name= "Observer"
    ; namespace= None
    ; description=
        [
          "This interface is used to create, update and destroy Observers to \
           control the use of tracing in different xapi components"
        ]
    ; version= (1, 0, 0)
    }

  let implementation = implement description

  let dbg_p = Param.mk ~name:"dbg" Types.string

  let unit_p = Param.mk ~name:"unit" Types.unit

  let endpoints_p = Param.mk ~name:"endpoints" (list Types.string)

  let bool_p = Param.mk ~name:"bool" Types.bool

  let uuid_p = Param.mk ~name:"uuid" Types.string

  let name_label_p = Param.mk ~name:"name_label" Types.string

  let dict_p = Param.mk ~name:"dict" dict

  let string_p = Param.mk ~name:"string" Types.string

  let int_p = Param.mk ~name:"int" Types.int

  let float_p = Param.mk ~name:"float" Types.float

  let create =
    declare "Observer.create" []
      (dbg_p
      @-> uuid_p
      @-> name_label_p
      @-> dict_p
      @-> endpoints_p
      @-> bool_p
      @-> returning unit_p err
      )

  let destroy =
    declare "Observer.destroy" [] (dbg_p @-> uuid_p @-> returning unit_p err)

  let set_enabled =
    declare "Observer.set_enabled" []
      (dbg_p @-> uuid_p @-> bool_p @-> returning unit_p err)

  let set_attributes =
    declare "Observer.set_attributes" []
      (dbg_p @-> uuid_p @-> dict_p @-> returning unit_p err)

  let set_endpoints =
    declare "Observer.set_endpoints" []
      (dbg_p @-> uuid_p @-> endpoints_p @-> returning unit_p err)

  let init = declare "Observer.init" [] (dbg_p @-> returning unit_p err)

  let set_trace_log_dir =
    declare "Observer.set_trace_log_dir" []
      (dbg_p @-> string_p @-> returning unit_p err)

  let set_export_interval =
    declare "Observer.set_export_interval" []
      (dbg_p @-> float_p @-> returning unit_p err)

  let set_max_spans =
    declare "Observer.set_max_spans" []
      (dbg_p @-> int_p @-> returning unit_p err)

  let set_max_traces =
    declare "Observer.set_max_traces" []
      (dbg_p @-> int_p @-> returning unit_p err)

  let set_max_file_size =
    declare "Observer.set_max_file_size" []
      (dbg_p @-> int_p @-> returning unit_p err)

  let set_host_id =
    declare "Observer.set_host_id" []
      (dbg_p @-> string_p @-> returning unit_p err)

  let set_compress_tracing_files =
    declare "Observer.set_compress_tracing_files" []
      (dbg_p @-> bool_p @-> returning unit_p err)
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

module Server (Impl : Server_impl) () = struct
  module S = ObserverAPI (Idl.Exn.GenServer ())

  let _ =
    S.create (fun dbg uuid name_label attributes endpoints enabled ->
        Impl.create () ~dbg ~uuid ~name_label ~attributes ~endpoints ~enabled
    ) ;
    S.destroy (fun dbg uuid -> Impl.destroy () ~dbg ~uuid) ;
    S.set_enabled (fun dbg uuid enabled ->
        Impl.set_enabled () ~dbg ~uuid ~enabled
    ) ;
    S.set_attributes (fun dbg uuid attributes ->
        Impl.set_attributes () ~dbg ~uuid ~attributes
    ) ;
    S.set_endpoints (fun dbg uuid endpoints ->
        Impl.set_endpoints () ~dbg ~uuid ~endpoints
    ) ;
    S.init (fun dbg -> Impl.init () ~dbg) ;
    S.set_trace_log_dir (fun dbg dir -> Impl.set_trace_log_dir () ~dbg ~dir) ;
    S.set_export_interval (fun dbg interval ->
        Impl.set_export_interval () ~dbg ~interval
    ) ;
    S.set_max_spans (fun dbg spans -> Impl.set_max_spans () ~dbg ~spans) ;
    S.set_max_traces (fun dbg traces -> Impl.set_max_traces () ~dbg ~traces) ;
    S.set_max_file_size (fun dbg file_size ->
        Impl.set_max_file_size () ~dbg ~file_size
    ) ;
    S.set_host_id (fun dbg host_id -> Impl.set_host_id () ~dbg ~host_id) ;
    S.set_compress_tracing_files (fun dbg enabled ->
        Impl.set_compress_tracing_files () ~dbg ~enabled
    )

  (* Bind all *)
  let process call = Idl.Exn.server S.implementation call
end

module Client = ObserverAPI (Idl.Exn.GenClient (struct
  let rpc call =
    Xcp_client.(
      retry_and_switch_rpc call ~use_switch:!use_switch ~queue_name
        ~dststr:queue_name ~uri
    )
end))
