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

module D = Debug.Make (struct let name = "create_storage" end)

open D

(* Return PBDs tha are current unplugged, raise alert if requested *)
let check_for_unplugged_pbds ~__context ~alert =
  let my_pbds = Helpers.get_my_pbds __context in
  let unplugged =
    my_pbds
    |> List.filter (fun (_, pbd_record) ->
           not pbd_record.API.pBD_currently_attached
       )
    |> List.map fst
  in
  if unplugged = [] then
    debug "All PBDs are plugged successfully"
  else (
    debug "Some PBDs are not currently plugged: %s"
      (String.concat ", " (List.map Ref.string_of unplugged)) ;
    if alert then
      let obj_uuid = Helpers.get_localhost_uuid () in
      Xapi_alert.add ~msg:Api_messages.pbd_plug_failed_on_server_start
        ~cls:`Host ~obj_uuid ~body:""
  ) ;
  unplugged

let plug_unplugged_pbds __context =
  (* If the plug is to succeed for SM's requiring a cluster stack
   * we have to enable the cluster stack too if we have one *)
  let my_pbds = Helpers.get_my_pbds __context in
  List.iter
    (fun (self, pbd_record) ->
      try
        if pbd_record.API.pBD_currently_attached then
          debug "Not replugging PBD %s: already plugged in" (Ref.string_of self)
        else
          Xapi_pbd.plug ~__context ~self
      with
      | Db_exn.DBCache_NotFound (_, "PBD", _) as e ->
          debug "Ignoring PBD/SR that got deleted before we plugged it: %s"
            (Printexc.to_string e)
      | e ->
          error "Could not plug in pbd '%s': %s" pbd_record.API.pBD_uuid
            (Printexc.to_string e)
      )
    my_pbds ;
  Xapi_host_helpers.consider_enabling_host ~__context

let plug_all_pbds __context =
  (* Explicitly resynchronise local PBD state *)
  let my_pbds = Helpers.get_my_pbds __context in
  Storage_access.resynchronise_pbds ~__context ~pbds:(List.map fst my_pbds) ;
  plug_unplugged_pbds __context

(* Create a PBD which connects this host to the SR, if one doesn't already exist *)
let maybe_create_pbd rpc session_id sr device_config me =
  let pbds = Client.SR.get_PBDs rpc session_id sr in
  let pbds =
    List.filter (fun self -> Client.PBD.get_host rpc session_id self = me) pbds
  in
  (* Check not more than 1 pbd in the database *)
  let pbds =
    if List.length pbds > 1 then (
      (* shouldn't happen... delete all but first pbd to make db consistent again *)
      List.iter (fun pbd -> Client.PBD.destroy rpc session_id pbd) (List.tl pbds) ;
      [List.hd pbds]
    ) else
      pbds
  in
  if List.length pbds = 0 (* If there's no PBD, create it *) then
    Client.PBD.create ~rpc ~session_id ~host:me ~sR:sr ~device_config
      ~other_config:[]
  else
    List.hd pbds

(* Otherwise, return the current one *)

let maybe_remove_tools_sr rpc session_id __context =
  let srs = Db.SR.get_all ~__context in
  let tools_srs =
    List.filter (fun self -> Db.SR.get_is_tools_sr ~__context ~self) srs
  in
  let old_srs =
    List.filter
      (fun self ->
        let other_config = Db.SR.get_other_config ~__context ~self in
        Db.SR.get_is_tools_sr ~__context ~self = false
        && (List.mem_assoc Xapi_globs.tools_sr_tag other_config
           || List.mem_assoc Xapi_globs.xensource_internal other_config
           )
        )
      srs
  in
  let unplug_and_maybe_destroy sr =
    (* Unplug me *)
    let pbd, _ = Sm.get_my_pbd_for_sr __context sr in
    Db.PBD.set_currently_attached ~__context ~self:pbd ~value:false ;
    (* Destroy any Tools SRs *)
    try
      Client.SR.forget rpc session_id sr ;
      info "Tools SR %s has been removed" (Ref.string_of sr)
    with
    | Api_errors.Server_error (e, _) when e = Api_errors.sr_has_pbd ->
        info
          "Tools SR %s could not be removed: it is still in use by another host"
          (Ref.string_of sr)
    | e ->
        warn "Failed to remove redundant Tools SR %s: %s" (Ref.string_of sr)
          (Printexc.to_string e)
  in
  List.iter unplug_and_maybe_destroy (tools_srs @ old_srs)

let initialise_storage (me : API.ref_host) rpc session_id __context : unit =
  let create_pbds_for_shared_srs () =
    let pool = List.hd (Client.Pool.get_all rpc session_id) in
    let master = Client.Pool.get_master rpc session_id pool in
    let srs = Client.SR.get_all_records_where rpc session_id "true" in
    let pbds = Client.PBD.get_all_records_where rpc session_id "true" in
    (* We want all shared SRs, but not the Tools SR (if any) if it is being removed *)
    let shared_srs =
      List.filter
        (fun (_, sr_rec) ->
          sr_rec.API.sR_shared
          && (!Xapi_globs.create_tools_sr || not sr_rec.API.sR_is_tools_sr)
          )
        srs
    in
    let shared_sr_refs = List.map fst shared_srs in
    let master_pbds =
      List.filter (fun (_, pbd_rec) -> pbd_rec.API.pBD_host = master) pbds
    in
    let maybe_create_pbd_for_shared_sr s =
      let mpbd, mpbd_rec =
        List.find (fun (_, pbdr) -> pbdr.API.pBD_SR = s) master_pbds
      in
      let master_devconf = mpbd_rec.API.pBD_device_config in
      let my_devconf = List.remove_assoc "SRmaster" master_devconf in
      (* this should never be used *)
      maybe_create_pbd rpc session_id s my_devconf me
    in
    List.iter
      (fun s -> try ignore (maybe_create_pbd_for_shared_sr s) with _ -> ())
      shared_sr_refs
  in
  let other_config =
    try
      let pool = Helpers.get_pool ~__context in
      Db.Pool.get_other_config ~__context ~self:pool
    with _ -> []
  in
  if
    not
      (List.mem_assoc Xapi_globs.sync_create_pbds other_config
      && List.assoc Xapi_globs.sync_create_pbds other_config
         = Xapi_globs.sync_switch_off
      )
  then (
    debug "Creating PBDs for shared SRs" ;
    create_pbds_for_shared_srs ()
  ) else
    debug "Skipping creation of PBDs for shared SRs" ;
  if not !Xapi_globs.create_tools_sr then
    maybe_remove_tools_sr rpc session_id __context ;
  plug_all_pbds __context ;
  let unplugged = check_for_unplugged_pbds ~__context ~alert:false in
  if unplugged <> [] then
    debug "%d PBDs remain unplugged, further attempts may be made later"
      (List.length unplugged)

let initialise_storage_localhost rpc session_id : unit =
  Server_helpers.exec_with_new_task "initialising storage" (fun context ->
      let me = Helpers.get_localhost ~__context:context in
      initialise_storage me rpc session_id context
  )
