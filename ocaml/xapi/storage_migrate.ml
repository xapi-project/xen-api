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

module D = Debug.Make (struct let name = "storage_migrate" end)

open D
module Listext = Xapi_stdext_std.Listext
open Xapi_stdext_pervasives.Pervasiveext
module Unixext = Xapi_stdext_unix.Unixext
open Storage_interface
open Storage_task
open Storage_migrate_helper

module type SMAPIv2_MIRROR = Storage_interface.MIRROR

let s_of_sr = Storage_interface.Sr.string_of

let choose_backend dbg sr =
  debug "%s dbg: %s choosing backend for sr :%s" __FUNCTION__ dbg (s_of_sr sr) ;
  match Storage_mux_reg.smapi_version_of_sr sr with
  | SMAPIv1 ->
      (module Storage_smapiv1_migrate.MIRROR : SMAPIv2_MIRROR)
  | SMAPIv3 ->
      (module Storage_smapiv3_migrate.MIRROR : SMAPIv2_MIRROR)
  | SMAPIv2 ->
      (* this should never happen *)
      failwith "unsupported SMAPI version smapiv2"

(** module [MigrateRemote] is similar to [MigrateLocal], but most of these functions
tend to be executed on the receiver side. *)
module MigrateRemote = struct
  let receive_start_common ~dbg ~sr ~vdi_info ~id ~similar ~vm =
    let on_fail : (unit -> unit) list ref = ref [] in
    let vdis = Local.SR.scan dbg sr in
    (* We drop cbt_metadata VDIs that do not have any actual data *)
    let vdis = List.filter (fun vdi -> vdi.ty <> "cbt_metadata") vdis in
    let leaf_dp = Local.DP.create dbg Uuidx.(to_string (make ())) in
    try
      let vdi_info = {vdi_info with sm_config= [("base_mirror", id)]} in
      let leaf = Local.VDI.create dbg sr vdi_info in
      info "Created leaf VDI for mirror receive: %s" (string_of_vdi_info leaf) ;
      on_fail := (fun () -> Local.VDI.destroy dbg sr leaf.vdi) :: !on_fail ;
      (* dummy VDI is created so that the leaf VDI becomes a differencing disk,
         useful for calling VDI.compose later on *)
      let dummy = Local.VDI.snapshot dbg sr leaf in
      on_fail := (fun () -> Local.VDI.destroy dbg sr dummy.vdi) :: !on_fail ;
      debug "%s Created dummy snapshot for mirror receive: %s" __FUNCTION__
        (string_of_vdi_info dummy) ;
      let _ : backend = Local.VDI.attach3 dbg leaf_dp sr leaf.vdi vm true in
      Local.VDI.activate3 dbg leaf_dp sr leaf.vdi vm ;
      let nearest =
        List.fold_left
          (fun acc content_id ->
            match acc with
            | Some _ ->
                acc
            | None -> (
              try
                Some
                  (List.find
                     (fun vdi ->
                       vdi.content_id = content_id
                       && vdi.virtual_size <= vdi_info.virtual_size
                     )
                     vdis
                  )
              with Not_found -> None
            )
          )
          None similar
      in
      debug "Nearest VDI: content_id=%s vdi=%s"
        (Option.fold ~none:"None" ~some:(fun x -> x.content_id) nearest)
        (Option.fold ~none:"None"
           ~some:(fun x -> Storage_interface.Vdi.string_of x.vdi)
           nearest
        ) ;
      let parent =
        match nearest with
        | Some vdi ->
            debug "Cloning VDI" ;
            let vdi = add_to_sm_config vdi "base_mirror" id in
            let vdi_clone = Local.VDI.clone dbg sr vdi in
            debug "Clone: %s" (Storage_interface.Vdi.string_of vdi_clone.vdi) ;
            ( if vdi_clone.virtual_size <> vdi_info.virtual_size then
                let new_size =
                  Local.VDI.resize dbg sr vdi_clone.vdi vdi_info.virtual_size
                in
                debug "Resize local clone VDI to %Ld: result %Ld"
                  vdi_info.virtual_size new_size
            ) ;
            vdi_clone
        | None ->
            debug "Creating a blank remote VDI" ;
            Local.VDI.create dbg sr vdi_info
      in
      debug "Parent disk content_id=%s" parent.content_id ;
      State.add id
        State.(
          Recv_op
            Receive_state.
              {
                sr
              ; dummy_vdi= dummy.vdi
              ; leaf_vdi= leaf.vdi
              ; leaf_dp
              ; parent_vdi= parent.vdi
              ; remote_vdi= vdi_info.vdi
              ; mirror_vm= vm
              }
        ) ;
      let nearest_content_id = Option.map (fun x -> x.content_id) nearest in
      Mirror.Vhd_mirror
        {
          Mirror.mirror_vdi= leaf
        ; mirror_datapath= leaf_dp
        ; copy_diffs_from= nearest_content_id
        ; copy_diffs_to= parent.vdi
        ; dummy_vdi= dummy.vdi
        }
    with e ->
      List.iter
        (fun op ->
          try op ()
          with e ->
            debug "Caught exception in on_fail: %s" (Printexc.to_string e)
        )
        !on_fail ;
      raise e

  let receive_start ~dbg ~sr ~vdi_info ~id ~similar =
    receive_start_common ~dbg ~sr ~vdi_info ~id ~similar ~vm:(Vm.of_string "0")

  let receive_start2 ~dbg ~sr ~vdi_info ~id ~similar ~vm =
    receive_start_common ~dbg ~sr ~vdi_info ~id ~similar ~vm

  let receive_finalize ~dbg ~id =
    let recv_state = State.find_active_receive_mirror id in
    let open State.Receive_state in
    Option.iter (fun r -> Local.DP.destroy dbg r.leaf_dp false) recv_state ;
    State.remove_receive_mirror id

  let receive_finalize2 ~dbg ~id =
    let recv_state = State.find_active_receive_mirror id in
    let open State.Receive_state in
    Option.iter
      (fun r ->
        SXM.info
          "%s Mirror done. Compose on the dest sr %s parent %s and leaf %s"
          __FUNCTION__ (Sr.string_of r.sr)
          (Vdi.string_of r.parent_vdi)
          (Vdi.string_of r.leaf_vdi) ;
        Local.DP.destroy2 dbg r.leaf_dp r.sr r.leaf_vdi r.mirror_vm false ;
        Local.VDI.compose dbg r.sr r.parent_vdi r.leaf_vdi ;
        (* On SMAPIv3, compose would have removed the now invalid dummy vdi, so
           there is no need to destroy it anymore, while this is necessary on SMAPIv1 SRs. *)
        log_and_ignore_exn (fun () -> Local.VDI.destroy dbg r.sr r.dummy_vdi) ;
        Local.VDI.remove_from_sm_config dbg r.sr r.leaf_vdi "base_mirror"
      )
      recv_state ;
    State.remove_receive_mirror id

  let receive_cancel ~dbg ~id =
    let receive_state = State.find_active_receive_mirror id in
    let open State.Receive_state in
    Option.iter
      (fun r ->
        log_and_ignore_exn (fun () -> Local.DP.destroy dbg r.leaf_dp false) ;
        List.iter
          (fun v -> log_and_ignore_exn (fun () -> Local.VDI.destroy dbg r.sr v))
          [r.dummy_vdi; r.leaf_vdi; r.parent_vdi]
      )
      receive_state ;
    State.remove_receive_mirror id
end

(** This module [MigrateLocal] consists of the concrete implementations of the 
migration part of SMAPI. Functions inside this module are sender driven, which means
they tend to be executed on the sender side. although there is not a hard rule
on what is executed on the sender side, this provides some heuristics. *)
module MigrateLocal = struct
  let stop_internal ~dbg ~id =
    (* Find the local VDI *)
    let alm = State.find_active_local_mirror id in
    match alm with
    | Some alm ->
        ( match alm.State.Send_state.remote_info with
        | Some remote_info -> (
            let sr, vdi = State.of_mirror_id id in
            let vdis = Local.SR.scan dbg sr in
            let local_vdi =
              match List.find_opt (fun x -> x.vdi = vdi) vdis with
              | None ->
                  failwith_fmt "Local VDI %s not found"
                    (Storage_interface.Vdi.string_of vdi)
              | Some v ->
                  v
            in
            let local_vdi = add_to_sm_config local_vdi "mirror" "null" in
            let local_vdi = remove_from_sm_config local_vdi "base_mirror" in
            (* Disable mirroring on the local machine *)
            let snapshot = Local.VDI.snapshot dbg sr local_vdi in
            Local.VDI.destroy dbg sr snapshot.vdi ;
            (* Destroy the snapshot, if it still exists *)
            let snap =
              List.find_opt
                (fun x -> List.assoc_opt "base_mirror" x.sm_config = Some id)
                vdis
            in
            ( match snap with
            | Some s ->
                debug "Found snapshot VDI: %s"
                  (Storage_interface.Vdi.string_of s.vdi) ;
                Local.VDI.destroy dbg sr s.vdi
            | None ->
                debug "Snapshot VDI already cleaned up"
            ) ;

            let (module Remote) =
              get_remote_backend remote_info.url remote_info.verify_dest
            in
            try Remote.DATA.MIRROR.receive_cancel dbg id with _ -> ()
          )
        | None ->
            ()
        ) ;
        State.remove_local_mirror id
    | None ->
        raise (Storage_interface.Storage_error (Does_not_exist ("mirror", id)))

  let stop ~dbg ~id =
    try stop_internal ~dbg ~id with
    | Storage_error (Backend_error (code, params))
    | Api_errors.Server_error (code, params) ->
        raise (Storage_error (Backend_error (code, params)))
    | e ->
        raise e

  let prepare ~dbg ~sr ~vdi ~dest ~local_vdi ~mirror_id ~mirror_vm ~url
      ~verify_dest =
    try
      let (module Remote) = get_remote_backend url verify_dest in
      let similars = similar_vdis ~dbg ~sr ~vdi in

      Remote.DATA.MIRROR.receive_start2 dbg dest local_vdi mirror_id similars
        mirror_vm
    with e ->
      error "%s Caught error %s while preparing for SXM" __FUNCTION__
        (Printexc.to_string e) ;
      raise
        (Storage_error (Migration_preparation_failure (Printexc.to_string e)))

  let start ~task ~dbg ~sr ~vdi ~dp ~mirror_vm ~copy_vm ~url ~dest ~verify_dest
      =
    SXM.info
      "%s sr:%s vdi:%s dp: %s mirror_vm: %s copy_vm: %s url:%s dest:%s \
       verify_dest:%B"
      __FUNCTION__
      (Storage_interface.Sr.string_of sr)
      (Storage_interface.Vdi.string_of vdi)
      dp
      (Storage_interface.Vm.string_of mirror_vm)
      (Storage_interface.Vm.string_of copy_vm)
      url
      (Storage_interface.Sr.string_of dest)
      verify_dest ;

    let (module Remote) = get_remote_backend url verify_dest in
    (* Find the local VDI *)
    let local_vdi = find_local_vdi ~dbg ~sr ~vdi in
    let mirror_id = State.mirror_id_of (sr, local_vdi.vdi) in
    debug "%s: Adding to active local mirrors before sending: id=%s"
      __FUNCTION__ mirror_id ;
    let alm =
      State.Send_state.
        {
          url
        ; dest_sr= dest
        ; remote_info= None
        ; local_dp= dp
        ; tapdev= None
        ; failed= false
        ; watchdog= None
        }
    in

    State.add mirror_id (State.Send_op alm) ;
    debug "%s Added mirror %s to active local mirrors" __FUNCTION__ mirror_id ;
    (* A list of cleanup actions to perform if the operation should fail. *)
    try
      let (Vhd_mirror remote_mirror) =
        prepare ~dbg ~sr ~vdi ~dest ~local_vdi ~mirror_id ~mirror_vm ~url
          ~verify_dest
      in
      let tapdev =
        Storage_smapiv1_migrate.mirror_pass_fds ~dbg ~dp ~sr ~vdi ~mirror_vm
          ~mirror_id ~url ~dest_sr:dest ~verify_dest ~remote_mirror
      in
      let snapshot =
        Storage_smapiv1_migrate.mirror_snapshot ~dbg ~sr ~dp ~mirror_id
          ~local_vdi
      in
      Storage_smapiv1_migrate.mirror_checker mirror_id tapdev ;
      let new_parent =
        Storage_smapiv1_migrate.mirror_copy ~task ~dbg ~sr ~snapshot ~copy_vm
          ~url ~dest_sr:dest ~remote_mirror ~verify_dest
      in
      debug "Local VDI %s = remote VDI %s"
        (Storage_interface.Vdi.string_of snapshot.vdi)
        (Storage_interface.Vdi.string_of new_parent.vdi) ;
      debug "Local VDI %s now mirrored to remote VDI: %s"
        (Storage_interface.Vdi.string_of local_vdi.vdi)
        (Storage_interface.Vdi.string_of remote_mirror.Mirror.mirror_vdi.vdi) ;
      debug "Destroying snapshot on src" ;
      Local.VDI.destroy dbg sr snapshot.vdi ;
      Some (Mirror_id mirror_id)
    with
    | Storage_error (Sr_not_attached sr_uuid) ->
        error " Caught exception %s:%s. Performing cleanup."
          Api_errors.sr_not_attached sr_uuid ;
        raise (Api_errors.Server_error (Api_errors.sr_not_attached, [sr_uuid]))
    | ( Storage_error (Migration_mirror_fd_failure reason)
      | Storage_error (Migration_mirror_snapshot_failure reason) ) as e ->
        error "%s: Caught %s: during storage migration preparation" __FUNCTION__
          reason ;
        MigrateRemote.receive_cancel ~dbg ~id:mirror_id ;
        raise e
    | Storage_error (Migration_mirror_copy_failure reason) as e ->
        error "%s: Caught %s: during storage migration copy" __FUNCTION__ reason ;
        stop ~dbg ~id:mirror_id ;
        raise e
    | e ->
        error "Caught %s during SXM: " (Api_errors.to_string e) ;
        stop ~dbg ~id:mirror_id ;
        raise e

  let stat ~dbg:_ ~id =
    let recv_opt = State.find_active_receive_mirror id in
    let send_opt = State.find_active_local_mirror id in
    let copy_opt = State.find_active_copy id in
    let open State in
    let failed =
      match send_opt with
      | Some send_state ->
          let failed =
            match send_state.Send_state.tapdev with
            | Some tapdev -> (
              try
                let stats = Tapctl.stats (Tapctl.create ()) tapdev in
                stats.Tapctl.Stats.nbd_mirror_failed = 1
              with _ ->
                debug "Using cached copy of failure status" ;
                send_state.Send_state.failed
            )
            | None ->
                false
          in
          send_state.Send_state.failed <- failed ;
          failed
      | None ->
          false
    in
    let state =
      (match recv_opt with Some _ -> [Mirror.Receiving] | None -> [])
      @ (match send_opt with Some _ -> [Mirror.Sending] | None -> [])
      @ match copy_opt with Some _ -> [Mirror.Copying] | None -> []
    in
    if state = [] then raise (Storage_error (Does_not_exist ("mirror", id))) ;
    let src, dst =
      match (recv_opt, send_opt, copy_opt) with
      | Some receive_state, _, _ ->
          ( receive_state.Receive_state.remote_vdi
          , receive_state.Receive_state.leaf_vdi
          )
      | _, Some send_state, _ ->
          let dst_vdi =
            match send_state.Send_state.remote_info with
            | Some remote_info ->
                remote_info.Send_state.vdi
            | None ->
                Storage_interface.Vdi.of_string ""
          in
          (snd (of_mirror_id id), dst_vdi)
      | _, _, Some copy_state ->
          (snd (of_copy_id id), copy_state.Copy_state.copy_vdi)
      | _ ->
          failwith "Invalid"
    in
    {Mirror.source_vdi= src; dest_vdi= dst; state; failed}

  let list ~dbg =
    let send_ops, recv_ops, copy_ops = State.map_of () in
    let get_ids map = List.map fst map in
    let ids =
      get_ids send_ops @ get_ids recv_ops @ get_ids copy_ops
      |> Listext.List.setify
    in
    List.map (fun id -> (id, stat ~dbg ~id)) ids

  let killall ~dbg =
    let send_ops, recv_ops, copy_ops = State.map_of () in
    List.iter
      (fun (id, send_state) ->
        debug "Send in progress: %s" id ;
        List.iter log_and_ignore_exn
          [
            (fun () -> stop ~dbg ~id)
          ; (fun () ->
              Local.DP.destroy dbg send_state.State.Send_state.local_dp true
            )
          ]
      )
      send_ops ;
    List.iter
      (fun (id, (copy_state : State.Copy_state.t)) ->
        debug "Copy in progress: %s" id ;
        List.iter log_and_ignore_exn
          [
            (fun () ->
              Local.DP.destroy dbg copy_state.State.Copy_state.leaf_dp true
            )
          ; (fun () ->
              Local.DP.destroy dbg copy_state.State.Copy_state.base_dp true
            )
          ] ;
        let (module Remote) =
          get_remote_backend copy_state.remote_url copy_state.verify_dest
        in
        List.iter log_and_ignore_exn
          [
            (fun () ->
              Remote.DP.destroy dbg copy_state.State.Copy_state.remote_dp true
            )
          ; (fun () ->
              Remote.VDI.destroy dbg copy_state.State.Copy_state.dest_sr
                copy_state.State.Copy_state.copy_vdi
            )
          ]
      )
      copy_ops ;
    List.iter
      (fun (id, _recv_state) ->
        debug "Receive in progress: %s" id ;
        log_and_ignore_exn (fun () -> Local.DATA.MIRROR.receive_cancel dbg id)
      )
      recv_ops ;
    State.clear ()
end

exception Timeout of Mtime.Span.t

let reqs_outstanding_timeout = Mtime.Span.(150 * s)

let pp_time () = Fmt.str "%a" Mtime.Span.pp

(* Tapdisk should time out after 2 mins. We can wait a little longer *)

let pre_deactivate_hook ~dbg:_ ~dp:_ ~sr ~vdi =
  let open State.Send_state in
  let id = State.mirror_id_of (sr, vdi) in
  let start = Mtime_clock.counter () in
  State.find_active_local_mirror id
  |> Option.iter (fun s ->
         (* We used to pause here and then check the nbd_mirror_failed key. Now, we poll
            					   until the number of outstanding requests has gone to zero, then check the
            					   status. This avoids confusing the backend (CA-128460) *)
         try
           match s.tapdev with
           | None ->
               ()
           | Some tapdev ->
               let open Tapctl in
               let ctx = create () in
               let rec wait () =
                 let elapsed = Mtime_clock.count start in
                 if Mtime.Span.compare elapsed reqs_outstanding_timeout > 0 then
                   raise (Timeout elapsed) ;
                 let st = stats ctx tapdev in
                 if st.Stats.reqs_outstanding > 0 then (
                   Thread.delay 1.0 ; wait ()
                 ) else
                   (st, elapsed)
               in
               let st, elapsed = wait () in
               debug "Got final stats after waiting %a" pp_time elapsed ;
               if st.Stats.nbd_mirror_failed = 1 then (
                 error "tapdisk reports mirroring failed" ;
                 s.failed <- true
               )
         with
         | Timeout elapsed ->
             error
               "Timeout out after %a waiting for tapdisk to complete all \
                outstanding requests"
               pp_time elapsed ;
             s.failed <- true
         | e ->
             error "Caught exception while finally checking mirror state: %s"
               (Printexc.to_string e) ;
             s.failed <- true
     )

let post_deactivate_hook ~sr ~vdi ~dp:_ =
  let open State.Send_state in
  let id = State.mirror_id_of (sr, vdi) in
  State.find_active_local_mirror id
  |> Option.iter (fun r ->
         let verify_dest =
           Option.fold ~none:false
             ~some:(fun ri -> ri.verify_dest)
             r.remote_info
         in
         let (module Remote) = get_remote_backend r.url verify_dest in
         debug "Calling receive_finalize2" ;
         log_and_ignore_exn (fun () ->
             Remote.DATA.MIRROR.receive_finalize2 "Mirror-cleanup" id
         ) ;
         debug "Finished calling receive_finalize2" ;
         State.remove_local_mirror id ;
         debug "Removed active local mirror: %s" id ;
         Option.iter (fun id -> Scheduler.cancel scheduler id) r.watchdog
     )

let nbd_handler req s ?(vm = "0") sr vdi dp =
  debug "%s: vm=%s sr=%s vdi=%s dp=%s" __FUNCTION__ vm sr vdi dp ;
  let sr, vdi = Storage_interface.(Sr.of_string sr, Vdi.of_string vdi) in
  req.Http.Request.close <- true ;
  let vm = Vm.of_string vm in
  let path =
    Storage_utils.transform_storage_exn (fun () ->
        Local.DATA.import_activate "nbd" dp sr vdi vm
    )
  in
  Http_svr.headers s (Http.http_200_ok () @ ["Transfer-encoding: nbd"]) ;
  let control_fd = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  finally
    (fun () ->
      Unix.connect control_fd (Unix.ADDR_UNIX path) ;
      let msg = dp in
      let len = String.length msg in
      let written = Unixext.send_fd_substring control_fd msg 0 len [] s in

      if written <> len then (
        error "Failed to transfer fd to %s" path ;
        Http_svr.headers s (Http.http_404_missing ~version:"1.0" ()) ;
        req.Http.Request.close <- true
      )
    )
    (fun () -> Unix.close control_fd)

(** nbd_proxy is a http handler but will turn the http connection into an nbd connection.
It proxies the connection between the sender and the generic nbd server, as returned
by [get_nbd_server dp sr vdi vm]. *)
let nbd_proxy req s vm sr vdi dp =
  debug "%s: vm=%s sr=%s vdi=%s dp=%s" __FUNCTION__ vm sr vdi dp ;
  let sr, vdi = Storage_interface.(Sr.of_string sr, Vdi.of_string vdi) in
  req.Http.Request.close <- true ;
  let vm = Vm.of_string vm in
  let path =
    Storage_utils.transform_storage_exn (fun () ->
        Local.DATA.get_nbd_server "nbd" dp sr vdi vm
    )
  in
  debug "%s got nbd server path %s" __FUNCTION__ path ;
  Http_svr.headers s (Http.http_200_ok () @ ["Transfer-encoding: nbd"]) ;
  let control_fd = Unixext.open_connection_unix_fd path in
  finally
    (fun () ->
      let s' = Unix.dup s in
      let control_fd' = Unix.dup control_fd in
      debug "%s: Connected; running proxy (between fds: %d and %d)" __FUNCTION__
        (Unixext.int_of_file_descr control_fd')
        (Unixext.int_of_file_descr s') ;
      Unixext.proxy s' control_fd' ;
      debug "%s: proxy exited" __FUNCTION__
    )
    (fun () -> Unix.close control_fd)

let with_dbg ~name ~dbg f =
  Debug_info.with_dbg ~with_thread:true ~module_name:__MODULE__ ~name ~dbg f

let with_task_and_thread ~dbg f =
  let task =
    Storage_task.add tasks dbg.Debug_info.log (fun task ->
        Storage_task.set_tracing task dbg.Debug_info.tracing ;
        try f task with
        | Storage_error (Backend_error (code, params))
        | Api_errors.Server_error (code, params) ->
            raise (Storage_error (Backend_error (code, params)))
        | Storage_error (Unimplemented msg) ->
            raise (Storage_error (Unimplemented msg))
        | e ->
            raise (Storage_error (Internal_error (Printexc.to_string e)))
    )
  in
  let _ =
    Thread.create
      (fun () ->
        Storage_task.run task ;
        signal (Storage_task.id_of_handle task)
      )
      ()
  in
  Storage_task.id_of_handle task

(* The following functions acts as wrappers of the migration part of SMAPIv2. Some of
   them are just direct calling of the functions inside the Migrate module. Leave it
   this way so that they all stay in one place rather than being spread around the
   file. *)

let copy ~dbg ~sr ~vdi ~vm ~url ~dest ~verify_dest =
  with_task_and_thread ~dbg (fun task ->
      Storage_smapiv1_migrate.Copy.copy_into_sr ~task ~dbg:dbg.Debug_info.log
        ~sr ~vdi ~vm ~url ~dest ~verify_dest
  )

let start ~dbg ~sr ~vdi ~dp ~mirror_vm ~copy_vm ~url ~dest ~verify_dest =
  with_dbg ~name:__FUNCTION__ ~dbg @@ fun dbg ->
  with_task_and_thread ~dbg (fun task ->
      MigrateLocal.start ~task ~dbg:dbg.Debug_info.log ~sr ~vdi ~dp ~mirror_vm
        ~copy_vm ~url ~dest ~verify_dest
  )

(* XXX: PR-1255: copy the xenopsd 'raise Exception' pattern *)
let stop ~dbg ~id =
  try MigrateLocal.stop ~dbg ~id with
  | Storage_error (Backend_error (code, params))
  | Api_errors.Server_error (code, params) ->
      raise (Storage_error (Backend_error (code, params)))
  | e ->
      raise e

let list = MigrateLocal.list

let killall = MigrateLocal.killall

let stat = MigrateLocal.stat

let receive_start = MigrateRemote.receive_start

let receive_start2 = MigrateRemote.receive_start2

let receive_finalize = MigrateRemote.receive_finalize

let receive_finalize2 = MigrateRemote.receive_finalize2

let receive_cancel = MigrateRemote.receive_cancel

(* The remote end of this call, SR.update_snapshot_info_dest, is implemented in
 * the SMAPIv1 section of storage_migrate.ml. It needs to access the setters
 * for snapshot_of, snapshot_time and is_a_snapshot, which we don't want to add
 * to SMAPI. *)
let update_snapshot_info_src ~dbg ~sr ~vdi ~url ~dest ~dest_vdi ~snapshot_pairs
    ~verify_dest =
  let (module Remote) = get_remote_backend url verify_dest in
  let local_vdis = Local.SR.scan dbg sr in
  let find_vdi ~vdi ~vdi_info_list =
    try List.find (fun x -> x.vdi = vdi) vdi_info_list
    with Not_found ->
      raise (Storage_error (Vdi_does_not_exist (Vdi.string_of vdi)))
  in
  let snapshot_pairs_for_remote =
    List.map
      (fun (local_snapshot, remote_snapshot) ->
        (remote_snapshot, find_vdi ~vdi:local_snapshot ~vdi_info_list:local_vdis)
      )
      snapshot_pairs
  in
  Remote.SR.update_snapshot_info_dest dbg dest dest_vdi
    (find_vdi ~vdi ~vdi_info_list:local_vdis)
    snapshot_pairs_for_remote
