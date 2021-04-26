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

(* Exception about systemctl operation like start/stop failed *)
exception Systemctl_fail of string

(* start a service with systemctl *)
val start : ?timeout:float -> wait_until_success:bool -> string -> unit

(* stop a service with systemctl *)
val stop : ?timeout:float -> wait_until_success:bool -> string -> unit
