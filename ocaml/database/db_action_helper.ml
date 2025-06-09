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

(* General DB utils *)

let __callback :
    (?snapshot:Rpc.t -> string -> string -> string -> unit) option ref =
  ref None

let events_register f = __callback := Some f

let events_notify ?snapshot ty op ref =
  match !__callback with None -> () | Some f -> f ?snapshot ty op ref
