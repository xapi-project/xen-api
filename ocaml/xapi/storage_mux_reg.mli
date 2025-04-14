(*
 * Copyright (c) Cloud Software Group
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

type processor = Rpc.call -> Rpc.response

val with_lock : Mutex.t -> (unit -> 'a) -> 'a

type plugin = {
    processor: processor
  ; backend_domain: string
  ; query_result: Storage_interface.query_result
  ; features: Smint.Feature.t list
}

val plugins : (Storage_interface.sr, plugin) Hashtbl.t

val debug_printer : ('a -> 'b) -> 'a -> 'b

val register :
     Storage_interface.sr
  -> (Rpc.call -> Rpc.response)
  -> string
  -> Storage_interface.query_result
  -> unit

val unregister : Storage_interface.sr -> unit

val sr_has_capability : Storage_interface.sr -> Smint.Feature.capability -> bool

val of_sr : Storage_interface.sr -> processor

val smapi_version_of_sr :
  Storage_interface.sr -> Storage_interface.smapi_version

type 'a sm_result = SMSuccess of 'a | SMFailure of exn

val s_of_sm_result : ('a -> string) -> 'a sm_result -> string

val success : 'a sm_result -> bool

val multicast :
     (Storage_interface.sr -> processor -> 'a)
  -> (Storage_interface.sr * 'a sm_result) list
