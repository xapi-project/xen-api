(*
* Copyright (C) 2024 Cloud Software Group
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

(** [Tracing_export] is a module dedicated for the creation and management of 
    threads that export the tracing data.
  *)

val set_export_interval : float -> unit
(** [set_export_interval seconds] sets the time interval between consecutive 
    exports of the finished spans to [seconds]. 

    Default is every [30.] seconds.
  *)

val set_host_id : string -> unit
(** [set_host_id id] sets the id of the host to [id]. 

    Default is ["localhost"].
  *)

val set_service_name : string -> unit
(** [set_service_name name] sets the name of the service to [name]. 
    All spans will be exported under this service's name. 
    
    Default name is ["unknown"].
  *)

(** [Destination] is a module for managing the export of tracing data to 
    different types of endpoints, whether is exporting it to a [File] or an 
    [Http] endpoint. 
    *)
module Destination : sig
  (** [File] is a module for managing the files in which the tracing data is 
       exported.
    *)
  module File : sig
    val set_max_file_size : int -> unit
    (** [set_max_file_size n] sets the maximum file size to [n]. If a file is
        is already created at the time of export and the file exceeds the
        maximum size, a new tracing file is created.
        *)

    val set_trace_log_dir : string -> unit
    (** [set_trace_log_dir log_dir] sets the location to which traces will be
        exported.

        Default is ["/var/log/dt/zipkinv2/json"]
      *)

    val get_trace_log_dir : unit -> string
    (** [get_trace_log_dir ()] returns the cuurent location to which traces are
        exported.
      *)

    val set_compress_tracing_files : bool -> unit
    (** [set_compress_tracing_files flag] sets wheater or not the tracing files
        are compressed or not.
      *)
  end

  val flush_spans : unit -> unit
  (** [flush_spans ()] forcefully flushes the spans to the current enabled 
        endpoints.
      *)

  (** [Http] is a module for managing exporting tracing data to an http 
       endpoint.
    *)
  module Http : sig
    val export : url:Uri.t -> string -> (unit, exn) result
    (** [export ~url json] forcefully flushes json formatted spans [json] to the
        given [url] .
      *)
  end
end

val flush_and_exit : unit -> unit
(** [flush_and_exit ()] sends a signal to flush the finish spans and terminate
    the exporter thread.
  *)

val main : unit -> Thread.t
(** [main ()] starts the exporter thread.
  *)
