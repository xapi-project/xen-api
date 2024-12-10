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

module D = Debug.Make (struct let name = "system_domains" end)

open D

(** If a VM is a system domain then xapi will perform lifecycle operations on demand,
    and will allow this VM to start even if a host is disabled. *)
let system_domain_key = "is_system_domain"

let bool_of_string x = try bool_of_string x with _ -> false

let is_system_domain snapshot =
  snapshot.API.vM_is_control_domain
  ||
  let oc = snapshot.API.vM_other_config in
  List.mem_assoc system_domain_key oc
  && bool_of_string (List.assoc system_domain_key oc)

let get_is_system_domain ~__context ~self =
  is_system_domain (Db.VM.get_record ~__context ~self)

(* Notes on other_config keys: in the future these should become first-class fields.
   For now note that although two threads may attempt to update these keys in parallel,
   order shouldn't matter because everyone will always update them to the same value.
   It's therefore safe to throw away exceptions. *)

let set_is_system_domain ~__context ~self ~value =
  Helpers.log_exn_continue
    (Printf.sprintf "set_is_system_domain self = %s" (Ref.string_of self))
    (fun () ->
      Db.VM.remove_from_other_config ~__context ~self ~key:system_domain_key ;
      Db.VM.add_to_other_config ~__context ~self ~key:system_domain_key ~value
    )
    ()

(** If a VM is a driver domain then it hosts backends for either disk or network
    devices. We link PBD.other_config:storage_driver_domain_key to
    VM.other_config:storage_driver_domain_key and we ensure the VM is marked as
    a system domain. *)
let storage_driver_domain_key = "storage_driver_domain"

let pbd_set_storage_driver_domain ~__context ~self ~value =
  Helpers.log_exn_continue
    (Printf.sprintf "pbd_set_storage_driver_domain self = %s"
       (Ref.string_of self)
    )
    (fun () ->
      Db.PBD.remove_from_other_config ~__context ~self
        ~key:storage_driver_domain_key ;
      Db.PBD.add_to_other_config ~__context ~self ~key:storage_driver_domain_key
        ~value
    )
    ()

let vm_set_storage_driver_domain ~__context ~self ~value =
  Helpers.log_exn_continue
    (Printf.sprintf "vm_set_storage_driver_domain self = %s" (Ref.string_of self)
    )
    (fun () ->
      Db.VM.remove_from_other_config ~__context ~self
        ~key:storage_driver_domain_key ;
      Db.VM.add_to_other_config ~__context ~self ~key:storage_driver_domain_key
        ~value
    )
    ()

let record_pbd_storage_driver_domain ~__context ~pbd ~domain =
  set_is_system_domain ~__context ~self:domain ~value:"true" ;
  pbd_set_storage_driver_domain ~__context ~self:pbd
    ~value:(Ref.string_of domain) ;
  vm_set_storage_driver_domain ~__context ~self:domain ~value:(Ref.string_of pbd)

let pbd_of_vm ~__context ~vm =
  let other_config = Db.VM.get_other_config ~__context ~self:vm in
  if List.mem_assoc storage_driver_domain_key other_config then
    Some (Ref.of_string (List.assoc storage_driver_domain_key other_config))
  else
    None

let storage_driver_domain_of_pbd ~__context ~pbd =
  let other_config = Db.PBD.get_other_config ~__context ~self:pbd in
  let dom0 = Helpers.get_domain_zero ~__context in
  if List.mem_assoc storage_driver_domain_key other_config then (
    let v = List.assoc storage_driver_domain_key other_config in
    if Db.is_valid_ref __context (Ref.of_string v) then
      Ref.of_string v
    else
      try Db.VM.get_by_uuid ~__context ~uuid:v
      with _ ->
        error "PBD %s has invalid %s key: falling back to dom0"
          (Ref.string_of pbd) storage_driver_domain_key ;
        dom0
  ) else
    dom0

let storage_driver_domain_of_pbd ~__context ~pbd =
  let domain = storage_driver_domain_of_pbd ~__context ~pbd in
  set_is_system_domain ~__context ~self:domain ~value:"true" ;
  pbd_set_storage_driver_domain ~__context ~self:pbd
    ~value:(Ref.string_of domain) ;
  vm_set_storage_driver_domain ~__context ~self:domain ~value:(Ref.string_of pbd) ;
  domain

let storage_driver_domain_of_vbd ~__context ~vbd =
  let dom0 = Helpers.get_domain_zero ~__context in
  let vdi = Db.VBD.get_VDI ~__context ~self:vbd in
  if Db.is_valid_ref __context vdi then
    let sr = Db.VDI.get_SR ~__context ~self:vdi in
    let sr_pbds = Db.SR.get_PBDs ~__context ~self:sr in
    let my_pbds = List.map fst (Helpers.get_my_pbds __context) in
    match Xapi_stdext_std.Listext.List.intersect sr_pbds my_pbds with
    | pbd :: _ ->
        storage_driver_domain_of_pbd ~__context ~pbd
    | _ ->
        dom0
  else
    dom0

let storage_driver_domain_of_sr_type ~__context ~_type =
  let dom0 = Helpers.get_domain_zero ~__context in
  dom0

let is_in_use ~__context ~self =
  let other_config = Db.VM.get_other_config ~__context ~self in
  List.mem_assoc storage_driver_domain_key other_config
  &&
  let pbd = Ref.of_string (List.assoc storage_driver_domain_key other_config) in
  if Db.is_valid_ref __context pbd then
    Db.PBD.get_currently_attached ~__context ~self:pbd
  else
    false

(* [wait_for ?timeout f] returns true if [f()] (called at 1Hz) returns true within
   the [timeout] period and false otherwise *)
let wait_for ?(timeout = 120.) f =
  let start = Unix.gettimeofday () in
  let finished = ref false in
  let success = ref false in
  while not !finished do
    let remaining = timeout -. (Unix.gettimeofday () -. start) in
    if remaining < 0. then
      finished := true
    else
      try
        if f () then (
          success := true ;
          finished := true
        ) else
          Thread.delay 1.
      with _ -> Thread.delay 1.
  done ;
  !success

let pingable ip () =
  try
    let (_ : string * string) =
      Forkhelpers.execute_command_get_output "/bin/ping"
        ["-c"; "1"; "-w"; "1"; ip]
    in
    true
  with _ -> false

let queryable ~__context transport () =
  let open Xmlrpc_client in
  let tracing = Context.set_client_span __context in
  let http = xmlrpc ~version:"1.0" "/" in
  let http = Helpers.TraceHelper.inject_span_into_req tracing http in
  let rpc =
    XMLRPC_protocol.rpc ~srcstr:"xapi" ~dststr:"remote_smapiv2" ~transport ~http
  in
  let listMethods = Rpc.call "system.listMethods" [] in
  try
    let _ = rpc listMethods in
    info "XMLRPC service found at %s" (string_of_transport transport) ;
    true
  with e ->
    debug "Temporary failure querying storage service on %s: %s"
      (string_of_transport transport)
      (Printexc.to_string e) ;
    false

let ip_of ~__context driver =
  (* Find the VIF on the Host internal management network *)
  let vifs = Db.VM.get_VIFs ~__context ~self:driver in
  let hin = Helpers.get_host_internal_management_network ~__context in
  let ip =
    let vif =
      try
        List.find
          (fun vif -> Db.VIF.get_network ~__context ~self:vif = hin)
          vifs
      with Not_found ->
        failwith
          (Printf.sprintf
             "driver domain %s has no VIF on host internal management network"
             (Ref.string_of driver)
          )
    in
    match Xapi_udhcpd.get_ip ~__context vif with
    | Some (a, b, c, d) ->
        Printf.sprintf "%d.%d.%d.%d" a b c d
    | None ->
        failwith
          (Printf.sprintf
             "driver domain %s has no IP on the host internal management \
              network"
             (Ref.string_of driver)
          )
  in
  info "driver domain uuid:%s ip:%s" (Db.VM.get_uuid ~__context ~self:driver) ip ;
  if not (wait_for (pingable ip)) then
    failwith
      (Printf.sprintf "driver domain %s is not responding to IP ping"
         (Ref.string_of driver)
      ) ;
  if not (wait_for (queryable ~__context (Xmlrpc_client.TCP (ip, 80)))) then
    failwith
      (Printf.sprintf "driver domain %s is not responding to XMLRPC query"
         (Ref.string_of driver)
      ) ;
  ip

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

let get_service service =
  with_lock service_to_queue_m (fun () ->
      Hashtbl.find_opt service_to_queue service
  )

let list_services () =
  with_lock service_to_queue_m (fun () ->
      Hashtbl.fold (fun service _ acc -> service :: acc) service_to_queue []
  )
