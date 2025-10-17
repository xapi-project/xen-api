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

let export_nbd_proxy ~remote_url ~mirror_vm ~sr ~vdi ~dp ~verify_dest =
  D.debug "%s spawning exporting nbd proxy" __FUNCTION__ ;
  let path =
    Printf.sprintf "/var/run/nbdproxy/export/%s" (Vm.string_of mirror_vm)
  in
  let proxy_srv = Fecomms.open_unix_domain_sock_server path in
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
    let verify_cert = if verify_dest then Stunnel_client.pool () else None in
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

let mirror_wait ~dbg ~sr ~vdi ~vm ~mirror_id mirror_key =
  let rec mirror_wait_rec key =
    let {failed; complete; progress} : Mirror.status =
      Local.DATA.stat dbg sr vdi vm key
    in
    if complete then (
      Option.fold ~none:()
        ~some:(fun p -> D.info "%s progress is %f" __FUNCTION__ p)
        progress ;
      D.info "%s qemu mirror %s completed" mirror_id __FUNCTION__
    ) else if failed then (
      Option.iter
        (fun (snd_state : State.Send_state.t) -> snd_state.failed <- true)
        (State.find_active_local_mirror mirror_id) ;
      D.info "%s qemu mirror %s failed" mirror_id __FUNCTION__ ;
      State.find_active_local_mirror mirror_id
      |> Option.iter (fun (s : State.Send_state.t) -> s.failed <- true) ;
      Updates.add (Dynamic.Mirror mirror_id) updates ;
      raise
        (Storage_interface.Storage_error
           (Migration_mirror_failure "Mirror failed during syncing")
        )
    ) else (
      Option.fold ~none:()
        ~some:(fun p -> D.info "%s progress is %f" __FUNCTION__ p)
        progress ;
      mirror_wait_rec key
    )
  in

  match mirror_key with
  | Storage_interface.Mirror.CopyV1 _ ->
      ()
  | Storage_interface.Mirror.MirrorV1 _ ->
      D.debug "%s waiting for mirroring to be done" __FUNCTION__ ;
      mirror_wait_rec mirror_key

module MIRROR : SMAPIv2_MIRROR = struct
  type context = unit

  let send_start _ctx ~dbg ~task_id:_ ~dp ~sr ~vdi ~mirror_vm ~mirror_id
      ~local_vdi:_ ~copy_vm:_ ~live_vm ~url ~remote_mirror ~dest_sr ~verify_dest
      =
    D.debug
      "%s dbg: %s dp: %s sr: %s vdi:%s mirror_vm:%s mirror_id: %s live_vm: %s \
       url:%s dest_sr:%s verify_dest:%B"
      __FUNCTION__ dbg dp (s_of_sr sr) (s_of_vdi vdi) (s_of_vm mirror_vm)
      mirror_id (s_of_vm live_vm) url (s_of_sr dest_sr) verify_dest ;
    ignore (Local.VDI.attach3 dbg dp sr vdi (Vm.of_string "0") true) ;
    (* TODO we are not activating the VDI here because SMAPIv3 does not support
       activating the VDI again on dom 0 when it is already activated on the live_vm.
       This means that if the VM shutsdown while SXM is in progress the
       mirroring for SMAPIv3 will fail.*)
    let nbd_proxy_path =
      Printf.sprintf "/var/run/nbdproxy/export/%s" (Vm.string_of mirror_vm)
    in
    match remote_mirror with
    | Mirror.Vhd_mirror _ ->
        raise
          (Storage_error
             (Migration_preparation_failure
                "Incorrect remote mirror format for SMAPIv3"
             )
          )
    | Mirror.SMAPIv3_mirror {nbd_export; mirror_datapath; mirror_vdi} -> (
      try
        let nbd_uri =
          Uri.make ~scheme:"nbd+unix" ~host:"" ~path:nbd_export
            ~query:[("socket", [nbd_proxy_path])]
            ()
          |> Uri.to_string
        in
        let _ : Thread.t =
          Thread.create
            (fun () ->
              export_nbd_proxy ~remote_url:url ~mirror_vm ~sr:dest_sr
                ~vdi:mirror_vdi.vdi ~dp:mirror_datapath ~verify_dest
            )
            ()
        in

        D.info "%s nbd_proxy_path: %s nbd_url %s" __FUNCTION__ nbd_proxy_path
          nbd_uri ;
        let mk = Local.DATA.mirror dbg sr vdi live_vm nbd_uri in

        D.debug "%s Updating active local mirrors: id=%s" __FUNCTION__ mirror_id ;
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
        mirror_wait ~dbg ~sr ~vdi ~vm:live_vm ~mirror_id mk
      with e ->
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

  let receive_start3 _ctx ~dbg ~sr ~vdi_info ~mirror_id ~similar:_ ~vm ~url
      ~verify_dest =
    D.debug "%s dbg: %s sr: %s vdi: %s id: %s vm: %s url: %s verify_dest: %B"
      __FUNCTION__ dbg (s_of_sr sr)
      (string_of_vdi_info vdi_info)
      mirror_id (s_of_vm vm) url verify_dest ;
    let module Remote = StorageAPI (Idl.Exn.GenClient (struct
      let rpc =
        Storage_utils.rpc ~srcstr:"smapiv2" ~dststr:"dst_smapiv2"
          (Storage_utils.connection_args_of_uri ~verify_dest url)
    end)) in
    let on_fail : (unit -> unit) list ref = ref [] in
    try
      (* We drop cbt_metadata VDIs that do not have any actual data *)
      let (vdi_info : vdi_info) =
        {vdi_info with sm_config= [("base_mirror", mirror_id)]}
      in
      let leaf_dp = Remote.DP.create dbg Uuidx.(to_string (make ())) in
      let leaf = Remote.VDI.create dbg sr vdi_info in
      D.info "Created leaf VDI for mirror receive: %s" (string_of_vdi_info leaf) ;
      on_fail := (fun () -> Remote.VDI.destroy dbg sr leaf.vdi) :: !on_fail ;
      let backend = Remote.VDI.attach3 dbg leaf_dp sr leaf.vdi vm true in
      let nbd_export =
        match nbd_export_of_attach_info backend with
        | None ->
            raise
              (Storage_error
                 (Migration_preparation_failure "Cannot parse nbd uri")
              )
        | Some export ->
            export
      in
      D.debug "%s activating dp %s sr: %s vdi: %s vm: %s" __FUNCTION__ leaf_dp
        (s_of_sr sr) (s_of_vdi leaf.vdi) (s_of_vm vm) ;
      Remote.VDI.activate3 dbg leaf_dp sr leaf.vdi vm ;
      let qcow2_res =
        {Mirror.mirror_vdi= leaf; mirror_datapath= leaf_dp; nbd_export}
      in
      let remote_mirror = Mirror.SMAPIv3_mirror qcow2_res in
      D.debug
        "%s updating receiving state lcoally to id: %s vm: %s vdi_info: %s"
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
