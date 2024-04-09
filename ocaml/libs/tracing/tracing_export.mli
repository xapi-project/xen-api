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

val set_export_interval : float -> unit

val set_host_id : string -> unit

val set_service_name : string -> unit

module Destination : sig
  module File : sig
    val set_max_file_size : int -> unit

    val set_trace_log_dir : string -> unit

    val get_trace_log_dir : unit -> string

    val set_compress_tracing_files : bool -> unit
  end

  val flush_spans : unit -> unit

  module Http : sig
    val export : url:Uri.t -> string -> (unit, exn) result
  end
end

val flush_and_exit : unit -> unit

val main : unit -> Thread.t
