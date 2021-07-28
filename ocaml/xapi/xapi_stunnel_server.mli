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

(** Wrapper around the stunnel@xapi systemd service.
    There exist scripts (e.g. xe-toolstack-restart) which also manipulate
    the stunnel daemon but they do this directly (not via ocaml). *)

val restart : __context:Context.t -> bool -> unit
(** restart stunnel, possibly changing the config file *)

val reload : unit -> unit
(** reload (potentially updated) configuration *)
