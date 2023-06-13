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

val update :
     rpc:(Rpc.call -> Rpc.response)
  -> session_id:[< `session] Ref.t
  -> alert_obj_description:string
  -> expired_message_id:string * int64
  -> expiring_conditions:(int * (string * int64)) list
  -> expiry:Xapi_stdext_date.Date.t
  -> msg_cls:
       [< `Certificate
       | `Host
       | `PVS_proxy
       | `Pool
       | `SR
       | `VDI
       | `VM
       | `VMPP
       | `VMSS ]
  -> msg_obj_uuid:string
  -> unit

(* Below exposed only for ease of testing *)

val message_body : string -> Xapi_stdext_date.Date.t -> string

val expired_message : string -> string

val expiring_message : string -> string

val generate_alert :
     float
  -> Xapi_stdext_date.Date.t
  -> 'a
  -> (int * 'a) list
  -> string
  -> (string * 'a) option

val update_message_internal :
     string * 'a
  -> ('b * (string * 'c)) list
  -> string
  -> string * (string * int64)
  -> ('d * API.message_t) list
  -> ('d * API.message_t) list * bool
