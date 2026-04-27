(*
 * Copyright (C) 2011 Citrix Systems Inc.
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
(**
 * @group Helper functions for handling system domains
*)

let with_lock = Xapi_stdext_threads.Threadext.Mutex.execute

(** If a VM is a system domain then xapi will perform lifecycle operations on demand,
    and will allow this VM to start even if a host is disabled. *)

let is_system_domain snapshot = snapshot.API.vM_is_control_domain
(* NOTE: code that recognises the other_config:is_system_domain key has been dropped *)

let get_is_system_domain ~__context ~self =
  is_system_domain (Db.VM.get_record ~__context ~self)

(* NOTE: the storage domain functionality used to be based on
   other-config:storage_driver_domain, which has been dropped *)

let pbd_of_vm ~__context:_ ~vm:_ = None

let storage_driver_domain_of_pbd ~__context ~pbd:_ =
  Helpers.get_domain_zero ~__context

let storage_driver_domain_of_vbd ~__context ~vbd:_ =
  Helpers.get_domain_zero ~__context

let storage_driver_domain_of_sr_type ~__context ~_type:_ =
  Helpers.get_domain_zero ~__context

type service = {uuid: string; ty: string; instance: string; url: string}
[@@deriving rpc]

type services = service list [@@deriving rpc]

let service_to_queue = Hashtbl.create 10

let service_to_queue_m = Mutex.create ()

let register_service service queue =
  with_lock service_to_queue_m (fun () ->
      Hashtbl.replace service_to_queue service queue
  )

let unregister_service service =
  with_lock service_to_queue_m (fun () ->
      Hashtbl.remove service_to_queue service
  )

let list_services () =
  with_lock service_to_queue_m (fun () ->
      Hashtbl.fold (fun service _ acc -> service :: acc) service_to_queue []
  )
