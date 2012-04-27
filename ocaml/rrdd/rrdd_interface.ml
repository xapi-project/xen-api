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

(*
 * The interface of the RRD daemon is defined by the extern function
 * declarations in this file. Implemented by RRD server (separate
 * thread), used by RRD client (part of xapi).
 *)

let name = "xcp-rrdd"

(* The interface is defined by extern function declarations. *)
external hello : string -> string = ""
