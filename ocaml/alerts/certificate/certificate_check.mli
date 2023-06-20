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

val alert : (Rpc.call -> Rpc.response) -> [< `session] Ref.t -> unit

(* Below exposed only for ease of testing *)

type cert =
  | CA of API.ref_Certificate * API.datetime
  | Host of API.ref_host * API.datetime
  | Internal of API.ref_host * API.datetime

val certificate_description : cert -> string

val message_sent_on_remaining_days_list : cert -> (int * (string * int64)) list

val get_expiry : cert -> Xapi_stdext_date.Date.t

val generate_alert :
     Xapi_stdext_date.Date.t
  -> string
  -> (int * (string * int64)) list
  -> Xapi_stdext_date.Date.t
  -> (string * (string * int64)) option
