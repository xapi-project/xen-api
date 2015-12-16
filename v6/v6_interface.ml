(*
 * Copyright (C) Citrix Systems Inc.
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

let service_name = "v6d"
let queue_name = ref (Xcp_service.common_prefix ^ service_name)
let default_sockets_dir = "/var/lib/xcp"
let default_path = ref (Filename.concat default_sockets_dir service_name)
let uri () = "file:" ^ !default_path

type debug_info = string

exception Invalid_edition of string
exception V6d_failure
exception License_expired
exception License_processing_error
exception Missing_connection_details
exception License_checkout_error of string

(* dbg_str -> requested edition -> current params ->
   new edition * new xapi params * new additional params *)
external apply_edition : debug_info -> string -> (string * string) list ->
	string * (string * string) list * (string * string) list = ""

(* dbg_str -> list of editions *)
external get_editions : debug_info -> (string * (string * string * int)) list = ""

(* dbg_str -> result *)
external get_version : debug_info -> string = ""
