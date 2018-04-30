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
(** Use the API to register a set of default SRs with the server.
 * @group Storage
*)

open Client
module D=Debug.Make(struct let name="xapi" end)
open D

let plug_all_pbds __context =
  (* Explicitly resynchronise local PBD state *)
  let my_pbds = Helpers.get_my_pbds __context in
  Storage_access.resynchronise_pbds ~__context ~pbds:(List.map fst my_pbds);
  let my_pbds = Helpers.get_my_pbds __context in
  (* We'll return true if all PBD.plugs succeed *)
  let result = ref true in
  List.iter
    (fun (self, pbd_record) ->
       try
         if pbd_record.API.pBD_currently_attached
         then debug "Not replugging PBD %s: already plugged in" (Ref.string_of self)
         else Xapi_pbd.plug ~__context ~self
       with e ->
         result := false;
         error "Could not plug in pbd '%s': %s" pbd_record.API.pBD_uuid (Printexc.to_string e))
    my_pbds;
  !result

let maybe_reenable_cluster_host __context =
  let host = Helpers.get_localhost __context in
  match Xapi_clustering.find_cluster_host ~__context ~host with
  | Some self ->
     Xapi_cluster_host.enable ~__context ~self
  | None -> ()

let plug_unplugged_pbds __context =
  (* If the plug is to succeed for SM's requiring a cluster stack
   * we have to enable the cluster stack too if we have one *)
  log_and_ignore_exn(fun () -> maybe_reenable_cluster_host __context);
  let my_pbds = Helpers.get_my_pbds __context in
  List.iter
    (fun (self, pbd_record) ->
       try
         if pbd_record.API.pBD_currently_attached
         then debug "Not replugging PBD %s: already plugged in" (Ref.string_of self)
         else Xapi_pbd.plug ~__context ~self
       with e -> debug "Could not plug in pbd '%s': %s" (Db.PBD.get_uuid ~__context ~self) (Printexc.to_string e))
    my_pbds

(* Create a PBD which connects this host to the SR, if one doesn't already exist *)
let maybe_create_pbd rpc session_id sr device_config me =
  let pbds = Client.SR.get_PBDs rpc session_id sr in
  let pbds = List.filter (fun self -> Client.PBD.get_host rpc session_id self = me) pbds in
  (* Check not more than 1 pbd in the database *)
  let pbds =
    if List.length pbds > 1
    then begin
      (* shouldn't happen... delete all but first pbd to make db consistent again *)
      List.iter (fun pbd->Client.PBD.destroy rpc session_id pbd) (List.tl pbds);
      [List.hd pbds]
    end
    else pbds
  in
  if List.length pbds = 0 (* If there's no PBD, create it *)
  then Client.PBD.create ~rpc ~session_id ~host:me ~sR:sr ~device_config ~other_config:[]
  else List.hd pbds (* Otherwise, return the current one *)

let create_storage (me: API.ref_host) rpc session_id __context : unit =
  let create_pbds_for_shared_srs () =
    let pool = List.hd (Client.Pool.get_all rpc session_id) in
    let master = Client.Pool.get_master rpc session_id pool in
    let srs = Client.SR.get_all_records_where rpc session_id "true" in
    let pbds = Client.PBD.get_all_records_where rpc session_id "true" in
    let shared_srs = List.filter (fun (_,sr_rec) -> sr_rec.API.sR_shared) srs in
    let shared_sr_refs = List.map fst shared_srs in
    let master_pbds = List.filter (fun (_,pbd_rec) -> pbd_rec.API.pBD_host = master) pbds in
    let maybe_create_pbd_for_shared_sr s =
      let mpbd,mpbd_rec = List.find (fun (_,pbdr)->pbdr.API.pBD_SR = s) master_pbds in
      let master_devconf = mpbd_rec.API.pBD_device_config in
      let my_devconf = List.remove_assoc "SRmaster" master_devconf in (* this should never be used *)
      maybe_create_pbd rpc session_id s my_devconf me
    in
    List.iter (fun s -> try ignore (maybe_create_pbd_for_shared_sr s) with _ -> ()) shared_sr_refs
  in

  let other_config =
    try
      let pool = Helpers.get_pool ~__context in
      Db.Pool.get_other_config ~__context ~self:pool
    with _ -> []
  in

  if not (List.mem_assoc Xapi_globs.sync_create_pbds other_config && (List.assoc Xapi_globs.sync_create_pbds other_config = Xapi_globs.sync_switch_off)) then
    begin
      debug "Creating PBDs for shared SRs";
      create_pbds_for_shared_srs()
    end
  else
    debug "Skipping creation of PBDs for shared SRs";

  let all_pbds_ok = plug_all_pbds __context in
  if not(all_pbds_ok) then begin
    let obj_uuid = Helpers.get_localhost_uuid () in
    Xapi_alert.add ~msg:Api_messages.pbd_plug_failed_on_server_start ~cls:`Host ~obj_uuid ~body:"";
  end;
  Xapi_host_helpers.consider_enabling_host ~__context


let create_storage_localhost rpc session_id : unit =
  Server_helpers.exec_with_new_task "creating storage"
    (fun context->
       let me = Helpers.get_localhost ~__context:context in
       create_storage me rpc session_id context)
