(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
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

(** An error reported to the user as "Error: <message>" *)
exception Cli_failure of string

val track :
     (API.task_t -> unit)
  -> (Rpc.call -> Rpc.response)
  -> API.ref_session
  -> API.ref_task
  -> unit
(** [track callback rpc session_id task] waits for [task] to finish, calling
    [callback] on its record at each change. *)

val result_from_task :
  (Rpc.call -> Rpc.response) -> API.ref_session -> API.ref_task -> unit

val wait_for_task_completion :
  (Rpc.call -> Rpc.response) -> API.ref_session -> API.ref_task -> unit

val track_http_operation :
     ?use_existing_task:API.ref_task
  -> ?progress_bar:bool
  -> Unix.file_descr
  -> (Rpc.call -> Rpc.response)
  -> API.ref_session
  -> (API.ref_task -> Cli_protocol.command)
  -> string
  -> string

val rewrite_provisioning_xml :
  (Rpc.call -> Rpc.response) -> API.ref_session -> API.ref_VM -> string -> unit

val get_default_sr_uuid :
  (Rpc.call -> Rpc.response) -> API.ref_session -> string option

val ref_convert : string -> string

val get_server_error : string -> string list -> (string * string list) option

val server_error : string -> string list -> Unix.file_descr -> unit

val user_says_yes : Unix.file_descr -> bool

type someone = Master | SpecificHost of API.ref_host

val uri_of_someone :
  (Rpc.call -> Rpc.response) -> API.ref_session -> someone -> string

val string_of_exn : exn -> string

val get_pool :
  rpc:(Rpc.call -> Rpc.response) -> session_id:API.ref_session -> API.ref_pool

val get_master :
  rpc:(Rpc.call -> Rpc.response) -> session_id:API.ref_session -> API.ref_host
