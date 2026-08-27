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

module D = Debug.Make (struct let name = __MODULE__ end)

module Unixext = Xapi_stdext_unix.Unixext
module State = Storage_migrate_helper.State
module SXM = Storage_migrate_helper.SXM
open Storage_interface
open Storage_task
open Xmlrpc_client
open Storage_migrate_helper

module type SMAPIv2_MIRROR = Storage_interface.MIRROR

let s_of_sr = Storage_interface.Sr.string_of

let s_of_vdi = Storage_interface.Vdi.string_of

let s_of_vm = Storage_interface.Vm.string_of

let mirror_poll_interval = 0.5

let nbd_proxy_path_of_vm vm =
  Printf.sprintf "/var/run/nbdproxy/export/%s" (Vm.string_of vm)

let export_nbd_proxy ~proxy_srv ~remote_url ~mirror_vm ~sr ~vdi ~dp ~verify_dest
    =
  D.debug "%s spawning exporting nbd proxy" __FUNCTION__ ;
  let path = nbd_proxy_path_of_vm mirror_vm in
  try
    let uri =
      Printf.sprintf "/services/SM/nbdproxy/import/%s/%s/%s/%s"
        (Vm.string_of mirror_vm) (Sr.string_of sr) (Vdi.string_of vdi) dp
    in

    let dest_url = Http.Url.set_path (Http.Url.of_string remote_url) uri in
    D.debug "%s now waiting for connection at %s" __FUNCTION__ path ;
    let nbd_client, _addr = Unix.accept proxy_srv in
    D.debug "%s connection accepted" __FUNCTION__ ;
    let request =
      Http.Request.make
        ~query:(Http.Url.get_query_params dest_url)
        ~version:"1.0" ~user_agent:"export_nbd_proxy" Http.Put uri
    in
    D.debug "%s making request to dest %s" __FUNCTION__
      (Http.Url.to_string dest_url) ;
    let verify_cert =
      if verify_dest then
        Stunnel_client.pool ()
      else
        None
    in
    let transport = Xmlrpc_client.transport_of_url ~verify_cert dest_url in
    with_transport ~stunnel_wait_disconnect:false transport
      (with_http request (fun (_response, s) ->
           D.debug "%s starting proxy" __FUNCTION__ ;
           Unixext.proxy (Unix.dup s) (Unix.dup nbd_client)
       )
      ) ;
    Unix.close proxy_srv
  with e ->
    D.debug "%s did not get connection due to %s, closing" __FUNCTION__
      (Printexc.to_string e) ;
    Unix.close proxy_srv ;
    raise e

let wait_for_mirror ~dbg ~task ~sr ~vdi ~vm ?mirror_id ~error_msg mirror_key =
  let on_failure () =
    match mirror_id with
    | Some mid ->
        State.find_active_local_mirror mid
        |> Option.iter (fun (s : State.Send_state.t) -> s.failed <- true) ;
        Updates.add (Dynamic.Mirror mid) updates
    | None ->
        ()
  in
  (* [task] is [None] when the caller holds no handle: the mirror then
     reports no progress and cannot be cancelled. *)
  let report p =
    D.debug "%s mirror progress: %.2f" __FUNCTION__ p ;
    Option.iter (fun t -> progress_callback 0.05 0.9 t p) task
  in
  let rec poll key =
    Option.iter Storage_task.check_cancelling task ;
    let {failed; complete; progress} : Mirror.status =
      Local.DATA.stat dbg sr vdi vm key
    in
    Option.iter report progress ;
    if complete then
      D.debug "%s mirror completed" __FUNCTION__
    else if failed then (
      on_failure () ;
      raise
        (Storage_interface.Storage_error (Migration_mirror_failure error_msg))
    ) else (
      Unix.sleepf mirror_poll_interval ;
      poll key
    )
  in
  match mirror_key with
  | Storage_interface.Mirror.CopyV1 _ ->
      (* Both callers start a mirror, so a copy key means the poll below is
         skipped and the caller wrongly sees a finished transfer. *)
      raise
        (Storage_interface.Storage_error
           (Internal_error "wait_for_mirror was given a copy key")
        )
  | Storage_interface.Mirror.MirrorV1 _ ->
      D.debug "%s waiting for mirror to complete" __FUNCTION__ ;
      poll mirror_key

let detach_snapshot_vdi ~dbg ~dp ~sr ~snapshot_vdi ~copy_vm =
  D.debug "%s detaching snapshot VDI %s" __FUNCTION__ (s_of_vdi snapshot_vdi) ;
  Fun.protect
    ~finally:(fun () -> Local.VDI.detach dbg dp sr snapshot_vdi copy_vm)
    (fun () -> Local.VDI.deactivate dbg dp sr snapshot_vdi copy_vm)

let create_destination_snapshot ~dbg ~dest_sr ~dest_url ~verify_dest
    ~dest_vdi_info ~src_content_id =
  let (module Remote) =
    Storage_migrate_helper.get_remote_backend dest_url verify_dest
  in
  D.debug "%s creating snapshot of destination VDI %s" __FUNCTION__
    (s_of_vdi dest_vdi_info.vdi) ;
  let dest_snapshot =
    Remote.VDI.snapshot dbg dest_sr {dest_vdi_info with sm_config= []}
  in
  D.debug "%s propagating src content_id %s onto dest snapshot %s" __FUNCTION__
    src_content_id
    (s_of_vdi dest_snapshot.vdi) ;
  Remote.VDI.set_content_id dbg dest_sr dest_snapshot.vdi src_content_id ;
  {dest_snapshot with content_id= src_content_id}

let mirror_snapshot_into_existing_dest ~dbg ~task ~sr ~snapshot_vdi_uuid
    ~dest_sr ~dest_url ~verify_dest ~copy_vm ~image_format ~dest_vdi_info
    ~nbd_uri =
  SXM.info "%s mirroring snapshot %s into VDI %s" __FUNCTION__ snapshot_vdi_uuid
    (s_of_vdi dest_vdi_info.vdi) ;

  let snapshot_vdi = Vdi.of_string snapshot_vdi_uuid in
  let dp = Uuidx.(to_string (make ())) in

  (* Capture content_id before attach: update_snapshot_info_dest asserts src
     and dest content_ids match. *)
  let src_content_id =
    try (Local.VDI.stat dbg sr snapshot_vdi).content_id
    with e ->
      D.warn "%s failed to stat src snapshot %s for content_id: %s" __FUNCTION__
        snapshot_vdi_uuid (Printexc.to_string e) ;
      ""
  in
  D.debug "%s captured src snapshot %s content_id=%s" __FUNCTION__
    snapshot_vdi_uuid src_content_id ;

  ignore (Local.VDI.attach3 dbg dp sr snapshot_vdi copy_vm false) ;
  Fun.protect
    ~finally:(fun () ->
      D.log_and_ignore_exn (fun () ->
          detach_snapshot_vdi ~dbg ~dp ~sr ~snapshot_vdi ~copy_vm
      )
    )
    (fun () ->
      Local.VDI.activate_readonly dbg dp sr snapshot_vdi copy_vm ;
      D.debug "%s starting QEMU mirror from snapshot %s" __FUNCTION__
        snapshot_vdi_uuid ;
      let mirror_key =
        Local.DATA.mirror dbg sr snapshot_vdi image_format copy_vm nbd_uri
      in
      wait_for_mirror ~dbg ~task ~sr ~vdi:snapshot_vdi ~vm:copy_vm
        ~error_msg:(Printf.sprintf "Snapshot %s mirror failed" snapshot_vdi_uuid)
        mirror_key
    ) ;

  let dest_snapshot =
    create_destination_snapshot ~dbg ~dest_sr ~dest_url ~verify_dest
      ~dest_vdi_info ~src_content_id
  in
  D.debug "%s destination snapshot created: %s" __FUNCTION__
    (s_of_vdi dest_snapshot.vdi) ;
  dest_snapshot

let nbd_export_of_attach_info backend =
  match Storage_interface.nbd_export_of_attach_info backend with
  | Some export ->
      export
  | None ->
      raise
        (Storage_error
           (Migration_preparation_failure "No NBD export found in attach info")
        )

let start_nbd_proxy_thread ~url ~mirror_vm ~dest_sr ~mirror_vdi ~mirror_datapath
    ~verify_dest =
  (* Listen before returning: the caller hands the socket path to qemu-dp as
     soon as we do, and a bound socket queues connections in its backlog
     without waiting for the thread to reach Unix.accept. *)
  let proxy_srv =
    Fecomms.open_unix_domain_sock_server (nbd_proxy_path_of_vm mirror_vm)
  in
  try
    Thread.create
      (fun () ->
        export_nbd_proxy ~proxy_srv ~remote_url:url ~mirror_vm ~sr:dest_sr
          ~vdi:mirror_vdi.vdi ~dp:mirror_datapath ~verify_dest
      )
      ()
  with e -> Unix.close proxy_srv ; raise e

let nbd_uri_of_export ~nbd_proxy_path export =
  Uri.make ~scheme:"nbd+unix" ~host:"" ~path:export
    ~query:[("socket", [nbd_proxy_path])]
    ()
  |> Uri.to_string

(** The VDI a mirror writes into: a clone of [dest_base], or a blank VDI. *)
let create_destination_vdi (module Remote : SMAPIv2) ~dbg ~dest_sr ~vdi_info
    ~dest_base =
  match dest_base with
  | None ->
      D.debug "%s creating a blank destination VDI" __FUNCTION__ ;
      Remote.VDI.create dbg dest_sr vdi_info
  | Some base ->
      let clone =
        Remote.VDI.clone dbg dest_sr (Remote.VDI.stat dbg dest_sr base)
      in
      D.debug "%s cloned VDI %s from base %s" __FUNCTION__ (s_of_vdi clone.vdi)
        (s_of_vdi base) ;
      (* A clone inherits the base's identity, not [vdi_info]'s. *)
      Remote.VDI.set_name_label dbg dest_sr clone.vdi vdi_info.name_label ;
      Remote.VDI.set_name_description dbg dest_sr clone.vdi
        vdi_info.name_description ;
      List.iter
        (fun (key, value) ->
          Remote.VDI.add_to_sm_config dbg dest_sr clone.vdi key value
        )
        vdi_info.sm_config ;
      let virtual_size =
        if clone.virtual_size >= vdi_info.virtual_size then
          clone.virtual_size
        else
          Remote.VDI.resize dbg dest_sr clone.vdi vdi_info.virtual_size
      in
      {
        clone with
        virtual_size
      ; name_label= vdi_info.name_label
      ; name_description= vdi_info.name_description
      ; sm_config= vdi_info.sm_config
      }

module Copy = struct
  let prepare_destination_vdi ~dbg ~dest_sr ~url ~verify_dest ~vm ~local_vdi
      ~dest_base =
    let (module Remote) = get_remote_backend url verify_dest in
    let head =
      create_destination_vdi
        (module Remote)
        ~dbg ~dest_sr
        ~vdi_info:{local_vdi with sm_config= []}
        ~dest_base
    in
    let dp = Uuidx.(to_string (make ())) in
    let cleanup () =
      D.debug "%s cleaning up destination VDI %s" __FUNCTION__
        (s_of_vdi head.vdi) ;
      D.log_and_ignore_exn (fun () ->
          Remote.VDI.deactivate dbg dp dest_sr head.vdi vm
      ) ;
      D.log_and_ignore_exn (fun () ->
          Remote.VDI.detach dbg dp dest_sr head.vdi vm
      ) ;
      D.log_and_ignore_exn (fun () -> Remote.VDI.destroy dbg dest_sr head.vdi)
    in
    (* [cleanup] is not armed until we return, so undo the clone here. *)
    let nbd_uri =
      try
        let backend = Remote.VDI.attach3 dbg dp dest_sr head.vdi vm true in
        (* Mirror target: a readonly datapath makes qemu-dp reject it. *)
        Remote.VDI.activate3 dbg dp dest_sr head.vdi vm ;
        nbd_uri_of_export ~nbd_proxy_path:(nbd_proxy_path_of_vm vm)
          (nbd_export_of_attach_info backend)
      with e ->
        D.error "%s failed to prepare destination VDI %s: %s" __FUNCTION__
          (s_of_vdi head.vdi) (Printexc.to_string e) ;
        cleanup () ;
        raise e
    in
    (head, dp, nbd_uri, cleanup)

  let copy_into_sr ~task ~dbg ~sr ~vdi ~vm ~url ~dest ~dest_base ~image_format
      ~verify_dest =
    SXM.info "%s sr:%s vdi:%s url:%s dest:%s dest_base:%s verify_dest:%B"
      __FUNCTION__ (s_of_sr sr) (s_of_vdi vdi) url (s_of_sr dest)
      (Option.fold ~none:"none" ~some:s_of_vdi dest_base)
      verify_dest ;
    try
      let local_vdi = Local.VDI.stat dbg sr vdi in
      let head, dp, nbd_uri, cleanup =
        prepare_destination_vdi ~dbg ~dest_sr:dest ~url ~verify_dest ~vm
          ~local_vdi ~dest_base
      in
      Fun.protect ~finally:cleanup (fun () ->
          let _ : Thread.t =
            start_nbd_proxy_thread ~url ~mirror_vm:vm ~dest_sr:dest
              ~mirror_vdi:head ~mirror_datapath:dp ~verify_dest
          in
          let dest_snapshot =
            mirror_snapshot_into_existing_dest ~dbg ~task:(Some task) ~sr
              ~snapshot_vdi_uuid:(s_of_vdi vdi) ~dest_sr:dest ~dest_url:url
              ~verify_dest ~copy_vm:vm ~image_format ~dest_vdi_info:head
              ~nbd_uri
          in
          Some (Vdi_info dest_snapshot)
      )
    with
    | Storage_error (Backend_error (code, params))
    | Api_errors.Server_error (code, params) ->
        raise (Storage_error (Backend_error (code, params)))
    | e ->
        raise (Storage_error (Internal_error (Printexc.to_string e)))
end

let assert_migratable ~__context ~vm_uuid ~active_vdis ~snapshot_vdis =
  let uuid self = Db.VDI.get_uuid ~__context ~self in
  let is_v3_vdi vdi =
    Storage_mux_reg.smapi_version_of_sr
      (Storage_interface.Sr.of_string
         (Db.SR.get_uuid ~__context ~self:(Db.VDI.get_SR ~__context ~self:vdi))
      )
    = SMAPIv3
  in
  let abort what items render =
    raise
      (Api_errors.Server_error
         ( Api_errors.operation_not_allowed
         , [
             Printf.sprintf "Cannot migrate VM %s: %s [%s]" vm_uuid what
               (String.concat "; " (List.map render items))
           ]
         )
      )
  in
  (* A VDI-level snapshot can be taken of a snapshot as well as of an active
     disk, so the whole snapshot tree below the VM's disks has to be walked. A
     snapshot lives on its origin's SR, so the SMAPI version is settled at the
     roots, and the snapshots the VM owns are roots in their own right. *)
  let rec hidden_below origin =
    Db.VDI.get_snapshots ~__context ~self:origin
    |> List.concat_map (fun s ->
        if List.mem s snapshot_vdis then
          []
        else
          (s, origin) :: hidden_below s
    )
  in
  let hidden =
    active_vdis @ snapshot_vdis
    |> List.sort_uniq compare
    |> List.filter is_v3_vdi
    |> List.concat_map hidden_below
  in
  if hidden <> [] then
    abort
      "it has VDI-level snapshots on SMAPIv3 SRs that are not part of any VM \
       snapshot and must be deleted before migrating:"
      hidden (fun (s, origin) ->
        Printf.sprintf "%s (snapshot of %s)" (uuid s) (uuid origin)
    ) ;
  (* The destination rebuilds a snapshot on top of its active disk, so that disk
     must still exist and must be migrating with it. *)
  let origin_of s = Db.VDI.get_snapshot_of ~__context ~self:s in
  let orphans =
    snapshot_vdis
    |> List.filter (fun s -> not (List.mem (origin_of s) active_vdis))
    |> List.filter is_v3_vdi
  in
  let deleted, detached =
    List.partition
      (fun s -> not (Db.is_valid_ref __context (origin_of s)))
      orphans
  in
  if deleted <> [] then
    abort
      "it has snapshot VDIs on SMAPIv3 SRs whose active disk has been deleted; \
       delete these snapshots before migrating:"
      deleted uuid ;
  if detached <> [] then
    abort
      "it has snapshot VDIs on SMAPIv3 SRs whose active disk is not part of \
       this migration:"
      detached (fun s ->
        Printf.sprintf "%s (active disk %s)" (uuid s) (uuid (origin_of s))
    )

module MIRROR : SMAPIv2_MIRROR = struct
  type context = unit

  let send_start _ctx ~dbg ~task_id:_ ~dp ~sr ~vdi ~image_format ~mirror_vm
      ~mirror_id ~local_vdi:_ ~copy_vm:_ ~live_vm ~url ~remote_mirror ~dest_sr
      ~verify_dest =
    D.debug
      "%s dbg: %s dp: %s sr: %s vdi:%s image_format:%s mirror_vm:%s mirror_id: \
       %s live_vm: %s url:%s dest_sr:%s verify_dest:%B"
      __FUNCTION__ dbg dp (s_of_sr sr) (s_of_vdi vdi) image_format
      (s_of_vm mirror_vm) mirror_id (s_of_vm live_vm) url (s_of_sr dest_sr)
      verify_dest ;
    ignore (Local.VDI.attach3 dbg dp sr vdi (Vm.of_string "0") true) ;
    (* TODO we are not activating the VDI here because SMAPIv3 does not support
       activating the VDI again on dom 0 when it is already activated on the live_vm.
       This means that if the VM shutsdown while SXM is in progress the
       mirroring for SMAPIv3 will fail.*)
    match remote_mirror with
    | Mirror.Vhd_mirror _ ->
        raise
          (Storage_error
             (Migration_preparation_failure
                "Incorrect remote mirror format for SMAPIv3"
             )
          )
    | Mirror.SMAPIv3_mirror {nbd_export; mirror_datapath; mirror_vdi} -> (
        let nbd_proxy_path = nbd_proxy_path_of_vm mirror_vm in
        let nbd_uri = nbd_uri_of_export ~nbd_proxy_path nbd_export in
        try
          let _ : Thread.t =
            start_nbd_proxy_thread ~url ~mirror_vm ~dest_sr ~mirror_vdi
              ~mirror_datapath ~verify_dest
          in
          D.info "%s nbd_proxy_path: %s nbd_url %s" __FUNCTION__ nbd_proxy_path
            nbd_uri ;
          let mk = Local.DATA.mirror dbg sr vdi image_format live_vm nbd_uri in

          D.debug "%s Updating active local mirrors: id=%s" __FUNCTION__
            mirror_id ;
          let alm =
            State.Send_state.
              {
                url
              ; dest_sr
              ; remote_info=
                  Some
                    {dp= mirror_datapath; vdi= mirror_vdi.vdi; url; verify_dest}
              ; local_dp= dp
              ; tapdev= None
              ; failed= false
              ; watchdog= None
              ; vdi
              ; live_vm
              ; mirror_key= Some mk
              }
          in
          State.add mirror_id (State.Send_op alm) ;
          D.debug "%s Updated mirror_id %s in the active local mirror"
            __FUNCTION__ mirror_id ;
          (* [send_start] is given a task id rather than a handle, so the
             leaf mirror is not cancellable from here. *)
          wait_for_mirror ~dbg ~task:None ~sr ~vdi ~vm:live_vm ~mirror_id
            ~error_msg:"Leaf VDI mirror failed during syncing" mk
        with
        | Storage_interface.Storage_error _ as e ->
            raise e
        | e ->
            D.error "%s caught exception during mirror: %s" __FUNCTION__
              (Printexc.to_string e) ;
            raise
              (Storage_interface.Storage_error
                 (Migration_mirror_failure (Printexc.to_string e))
              )
      )

  let receive_start _ctx ~dbg:_ ~sr:_ ~vdi_info:_ ~id:_ ~similar:_ =
    Storage_interface.unimplemented __FUNCTION__

  let receive_start2 _ctx ~dbg:_ ~sr:_ ~vdi_info:_ ~id:_ ~similar:_ ~vm:_ =
    Storage_interface.unimplemented __FUNCTION__

  let receive_start3 _ctx ~dbg ~sr ~vdi_info ~mirror_id ~image_format ~similar:_
      ~vm ~url ~verify_dest ~dest_base =
    D.debug
      "%s dbg: %s sr: %s vdi: %s id: %s image_format: %s vm: %s url: %s \
       verify_dest: %B dest_base: %s"
      __FUNCTION__ dbg (s_of_sr sr)
      (string_of_vdi_info vdi_info)
      mirror_id image_format (s_of_vm vm) url verify_dest
      (Option.fold ~none:"none" ~some:s_of_vdi dest_base) ;
    let (module Remote) = get_remote_backend url verify_dest in
    let on_fail : (unit -> unit) list ref = ref [] in
    try
      (* We drop cbt_metadata VDIs that do not have any actual data *)
      let (vdi_info : vdi_info) =
        {vdi_info with sm_config= [("base_mirror", mirror_id)]}
      in
      let leaf_dp = Remote.DP.create dbg Uuidx.(to_string (make ())) in
      let leaf =
        create_destination_vdi
          (module Remote)
          ~dbg ~dest_sr:sr ~vdi_info ~dest_base
      in
      D.info "Created leaf VDI for mirror receive: %s" (string_of_vdi_info leaf) ;
      on_fail := (fun () -> Remote.VDI.destroy dbg sr leaf.vdi) :: !on_fail ;
      let backend = Remote.VDI.attach3 dbg leaf_dp sr leaf.vdi vm true in
      let nbd_export = nbd_export_of_attach_info backend in
      D.debug "%s activating dp %s sr: %s vdi: %s vm: %s" __FUNCTION__ leaf_dp
        (s_of_sr sr) (s_of_vdi leaf.vdi) (s_of_vm vm) ;
      Remote.VDI.activate3 dbg leaf_dp sr leaf.vdi vm ;
      let qcow2_res =
        {Mirror.mirror_vdi= leaf; mirror_datapath= leaf_dp; nbd_export}
      in
      let remote_mirror = Mirror.SMAPIv3_mirror qcow2_res in
      D.debug
        "%s updating receiving state locally to id: %s vm: %s vdi_info: %s"
        __FUNCTION__ mirror_id (s_of_vm vm)
        (string_of_vdi_info vdi_info) ;
      State.add mirror_id
        State.(
          Recv_op
            Receive_state.
              {
                sr
              ; leaf_vdi= qcow2_res.mirror_vdi.vdi
              ; leaf_dp= qcow2_res.mirror_datapath
              ; remote_vdi= vdi_info.vdi
              ; mirror_vm= vm
              ; dummy_vdi=
                  Vdi.of_string "dummy"
                  (* No dummy_vdi is needed when migrating from SMAPIv3 SRs, having a
                     "dummy" VDI here is fine as cleanup code for SMAPIv3 will not
                     access dummy_vdi, and all the clean up functions will ignore
                     exceptions when trying to clean up the dummy VDIs even if they
                     do access dummy_vdi. The same applies to parent_vdi *)
              ; parent_vdi= Vdi.of_string "dummy"
              ; url
              ; verify_dest
              }
        ) ;
      remote_mirror
    with e ->
      List.iter
        (fun op ->
          try op ()
          with e ->
            D.warn "Caught exception in on_fail: %s performing cleaning up"
              (Printexc.to_string e)
        )
        !on_fail ;
      raise e

  let receive_finalize _ctx ~dbg:_ ~id:_ =
    Storage_interface.unimplemented __FUNCTION__

  let receive_finalize2 _ctx ~dbg:_ ~id:_ =
    Storage_interface.unimplemented __FUNCTION__

  let receive_finalize3 _ctx ~dbg ~mirror_id ~sr ~url ~verify_dest =
    D.debug "%s dbg:%s id: %s sr: %s url: %s verify_dest: %B" __FUNCTION__ dbg
      mirror_id (s_of_sr sr) url verify_dest ;
    let (module Remote) =
      Storage_migrate_helper.get_remote_backend url verify_dest
    in
    let open State.Receive_state in
    let recv_state = State.find_active_receive_mirror mirror_id in
    Option.iter
      (fun r ->
        Remote.DP.destroy2 dbg r.leaf_dp r.sr r.leaf_vdi r.mirror_vm false ;
        Remote.VDI.remove_from_sm_config dbg r.sr r.leaf_vdi "base_mirror"
      )
      recv_state ;
    State.remove_receive_mirror mirror_id

  let receive_cancel _ctx ~dbg:_ ~id:_ =
    Storage_interface.unimplemented __FUNCTION__

  let list _ctx = Storage_interface.unimplemented __FUNCTION__

  let stat _ctx = Storage_interface.unimplemented __FUNCTION__

  let receive_cancel2 _ctx ~dbg ~mirror_id ~url ~verify_dest =
    D.debug "%s dbg:%s mirror_id:%s url:%s verify_dest:%B" __FUNCTION__ dbg
      mirror_id url verify_dest ;
    let (module Remote) =
      Storage_migrate_helper.get_remote_backend url verify_dest
    in
    let receive_state = State.find_active_receive_mirror mirror_id in
    let open State.Receive_state in
    Option.iter
      (fun r ->
        D.log_and_ignore_exn (fun () -> Remote.DP.destroy dbg r.leaf_dp false) ;
        D.log_and_ignore_exn (fun () -> Remote.VDI.destroy dbg r.sr r.leaf_vdi)
      )
      receive_state ;
    State.remove_receive_mirror mirror_id

  let has_mirror_failed _ctx ~dbg ~mirror_id ~sr =
    match State.find_active_local_mirror mirror_id with
    | Some ({mirror_key= Some mk; vdi; live_vm; _} : State.Send_state.t) ->
        let {failed; _} : Mirror.status =
          Local.DATA.stat dbg sr vdi live_vm mk
        in
        failed
    | _ ->
        false

  (* TODO currently we make the pre_deactivate_hook for SMAPIv3 a noop while for
     SMAPIv1 it will do a final check of the state of the mirror and report error
     if there is a mirror failure. We leave this for SMAPIv3 because the Data.stat
     call, which checks for the state of the mirror stops working once the domain
     has been paused, which happens before VDI.deactivate, hence we cannot do this check in
     pre_deactivate_hook. Instead we work around this by doing mirror check in mirror_wait
     as we repeatedly poll the state of the mirror job. In the future we might
     want to invent a different hook that can be called to do a final check just
     before the VM is paused. *)
  let pre_deactivate_hook _ctx ~dbg ~dp ~sr ~vdi =
    D.debug "%s dbg: %s dp: %s sr: %s vdi: %s" __FUNCTION__ dbg dp (s_of_sr sr)
      (s_of_vdi vdi)
end
