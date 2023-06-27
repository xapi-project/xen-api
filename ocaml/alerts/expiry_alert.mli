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

type message_name = string

type message_priority = int64

(* message_id is the type of mesage defined in Api_messages *)
type message_id = message_name * message_priority

type remaining_days = int

type raw_alert = {
    cls:
      [ `Certificate
      | `Host
      | `PVS_proxy
      | `Pool
      | `SR
      | `VDI
      | `VM
      | `VMPP
      | `VMSS ]
        (* parameter "cls" of XenAPI.Message.create *)
  ; obj_uuid: string (* parameter "obj_uuid" of XenAPI.Message.create *)
  ; obj_description: string
        (* description of the obj which would expire, which will be used in the body of
         * generated message, take host server certificate for example: "TLS server
         * certificate"
         *)
  ; alert_conditions: (remaining_days * message_id) list
        (* for example:
         *   [
         *     (0, Api_messages.host_server_certificate_expired)
         *   ; (7, Api_messages.host_server_certificate_expiring_07)
         *   ; (14, Api_messages.host_server_certificate_expiring_14)
         *   ; (30, Api_messages.host_server_certificate_expiring_30)
         *   ]
         * this means when the remaining days before it will expired is less than 30
         * days, message "host_server_certificate_expiring_30" should be sent, when
         * the remaining days is less than 14 days, message
         * "host_server_certificate_expiring_14" should be sent, when the remaining days
         * is less than 7 days, message "host_server_certificate_expiring_07" should be
         * sent, and when it has expired, message "host_server_certificate_expired"
         * should be sent.
         *)
  ; expiry: Xapi_stdext_date.Date.t
        (* the last second when the obj is valid, it will expire in the next second *)
}

val alert :
     rpc:(Rpc.call -> Rpc.response)
  -> session_id:[< `session] Ref.t
  -> raw_alert list
  -> unit
(** Update XenAPI messages as expiry alert: remove outdated messages and send a message
    when it has not been sent yet *)

(* Below exposed only for ease of testing *)

val message_body : string -> Xapi_stdext_date.Date.t -> string

val expired_message : string -> string

val expiring_message : string -> string

val maybe_generate_alert :
     Xapi_stdext_date.Date.t
  -> string
  -> (remaining_days * message_id) list
  -> Xapi_stdext_date.Date.t
  -> (string * message_id) option
(** An inner function exposed only for ease of testing, it returns None if there is no
    need for an expiry alert as it is not expiring or expired yet, otherwise an "alert"
    is returned in the form of (message_body, message_id) *)

val filter_messages :
     string list
  -> string
  -> string * (string * int64)
  -> ('c * API.message_t) list
  -> ('c * API.message_t) list * ('c * API.message_t) list
(** An inner function exposed only for ease of testing, it filters all of the existing
    XenAPI messages and returns the filtered messages in the form: (outdated, current),
    where "outdated" is a list of outdated XenAPI messages which need to be removed,
    "current" is a list of one element which is an XenAPI message exactly the same as
    the alert needs to be sent *)
