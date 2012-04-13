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
(** 'Real' network backend
 * @group Networking
 *)

(** Local IP address of the HIMN (if any) *)
val himn_addr : string option ref

(** Set-up a host internal management network *)
val maybe_start : string -> (string * string) list -> unit

(** Shutdown a host internal management network *)
val maybe_stop : string -> unit

(** On server start we may need to restart the proxy *)
val on_server_start : unit -> unit
