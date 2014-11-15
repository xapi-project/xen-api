(*
 * Copyright (C) 2006-2014 Citrix Systems Inc.
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

(* This callback is used in the events.next implementation *)

type snapshot_fn = unit -> Rpc.t option

type callback = ?snapshot_fn:snapshot_fn -> string -> string -> string -> unit

let __callback : callback option ref = ref None

let events_register f = __callback := Some f
let events_unregister () = __callback := None
    
let events_notify ?snapshot_fn ty op ref =
  match !__callback with
    | None -> ()
    | Some f -> f ?snapshot_fn ty op ref
