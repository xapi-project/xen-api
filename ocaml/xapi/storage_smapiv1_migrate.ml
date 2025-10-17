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

module D = Debug.Make (struct let name = "storage_smapiv1_migrate" end)

module Unixext = Xapi_stdext_unix.Unixext
open Xapi_stdext_pervasives.Pervasiveext
open Storage_interface
open Xmlrpc_client
open Storage_migrate_helper
open Storage_task
module State = Storage_migrate_helper.State
module SXM = Storage_migrate_helper.SXM

module type SMAPIv2_MIRROR = Storage_interface.MIRROR

let s_of_sr = Storage_interface.Sr.string_of

let s_of_vdi = Storage_interface.Vdi.string_of

let s_of_vm = Storage_interface.Vm.string_of

let with_activated_disk ~dbg ~sr ~vdi ~dp ~vm f =
  let attached_vdi =
    Option.map
      (fun vdi ->
        let backend = Local.VDI.attach3 dbg dp sr vdi vm false in
        (vdi, backend)
      )
      vdi
  in
  finally
    (fun () ->
      let path_and_nbd =
        Option.map
          (fun (vdi, backend) ->
            let _xendisks, blockdevs, files, nbds =
              Storage_interface.implementations_of_backend backend
            in
            match (files, blockdevs, nbds) with
            | {path} :: _, _, _ | _, {path} :: _, _ ->
                Local.VDI.activate3 dbg dp sr vdi vm ;
                (path, false)
            | _, _, nbd :: _ ->
                Local.VDI.activate3 dbg dp sr vdi vm ;
                let unix_socket_path, export_name =
                  Storage_interface.parse_nbd_uri nbd
                in
                ( Attach_helpers.NbdClient.start_nbd_client ~unix_socket_path
                    ~export_name
                , true
                )
            | [], [], [] ->
                raise
                  (Storage_interface.Storage_error
                     (Backend_error
                        ( Api_errors.internal_error
                        , [
                            "No File, BlockDevice or Nbd implementation in \
                             Datapath.attach response: "
                            ^ (Storage_interface.(rpc_of backend) backend
                              |> Jsonrpc.to_string
                              )
                          ]
                        )
                     )
                  )
          )
          attached_vdi
      in
      finally
        (fun () -> f (Option.map (function path, _ -> path) path_and_nbd))
        (fun () ->
          Option.iter
            (function
              | path, true ->
                  Attach_helpers.NbdClient.stop_nbd_client ~nbd_device:path
              | _ ->
                  ()
              )
            path_and_nbd ;
          Option.iter (fun vdi -> Local.VDI.deactivate dbg dp sr vdi vm) vdi
        )
    )
    (fun () ->
      Option.iter
        (fun (vdi, _) -> Local.VDI.detach dbg dp sr vdi vm)
        attached_vdi
    )

let tapdisk_of_attach_info (backend : Storage_interface.backend) =
  let _, blockdevices, _, nbds =
    Storage_interface.implementations_of_backend backend
  in
  match (blockdevices, nbds) with
  | blockdevice :: _, _ -> (
      let path = blockdevice.Storage_interface.path in
      try
        match Tapctl.of_device (Tapctl.create ()) path with
        | tapdev, _, _ ->
            Some tapdev
      with
      | Tapctl.Not_blktap ->
          D.debug "Device %s is not controlled by blktap" path ;
          None
      | Tapctl.Not_a_device ->
          D.debug "%s is not a device" path ;
          None
      | _ ->
          D.debug "Device %s has an unknown driver" path ;
          None
    )
  | _, nbd :: _ -> (
    try
      let path, _ = Storage_interface.parse_nbd_uri nbd in
      let filename = Unix.realpath path |> Filename.basename in
      Scanf.sscanf filename "nbd%d.%d" (fun pid minor ->
          Some (Tapctl.tapdev_of ~pid ~minor)
      )
    with _ ->
      D.debug "No tapdisk found for NBD backend: %s" nbd.Storage_interface.uri ;
      None
  )
  | _ ->
      D.debug "No tapdisk found for backend: %s"
        (Storage_interface.(rpc_of backend) backend |> Rpc.to_string) ;
      None

let progress_callback start len t y =
  let new_progress = start +. (y *. len) in
  Storage_task.set_state t (Task.Pending new_progress) ;
  signal (Storage_task.id_of_handle t)

let perform_cleanup_actions =
  List.iter (fun f ->
      try f ()
      with e ->
        D.error "Caught %s while performing cleanup actions"
          (Printexc.to_string e)
  )

module Copy = struct
  (** [copy_into_vdi] is similar to [copy_into_sr] but requires a [dest_vdi] parameter *)
  let copy_into_vdi ~task ~dbg ~sr ~vdi ~vm ~url ~dest ~dest_vdi ~verify_dest =
    let (module Remote) = get_remote_backend url verify_dest in
    D.debug "copy local=%s/%s url=%s remote=%s/%s verify_dest=%B"
      (Storage_interface.Sr.string_of sr)
      (Storage_interface.Vdi.string_of vdi)
      url
      (Storage_interface.Sr.string_of dest)
      (Storage_interface.Vdi.string_of dest_vdi)
      verify_dest ;
    (* Check the remote SR exists *)
    let srs = Remote.SR.list dbg in
    if not (List.mem dest srs) then
      failwith
        (Printf.sprintf "Remote SR %s not found"
           (Storage_interface.Sr.string_of dest)
        ) ;

    let remote_vdi, _ = find_vdi ~dbg ~sr:dest ~vdi:dest_vdi (module Remote) in
    let dest_content_id = remote_vdi.content_id in
    (* Find the local VDI *)
    let local_vdi, vdis = find_vdi ~dbg ~sr ~vdi (module Local) in
    D.debug "copy local content_id=%s" local_vdi.content_id ;
    D.debug "copy remote content_id=%s" dest_content_id ;
    if local_vdi.virtual_size > remote_vdi.virtual_size then (
      (* This should never happen provided the higher-level logic is working properly *)
      D.error "copy local virtual_size=%Ld > remote virtual_size = %Ld"
        local_vdi.virtual_size remote_vdi.virtual_size ;
      failwith "local VDI is larger than the remote VDI"
    ) ;
    let on_fail : (unit -> unit) list ref = ref [] in
    let base_vdi =
      try
        let x =
          (List.find (fun x -> x.content_id = dest_content_id) vdis).vdi
        in
        D.debug
          "local VDI has content_id = %s; we will perform an incremental copy"
          dest_content_id ;
        Some x
      with _ ->
        D.debug "no local VDI has content_id = %s; we will perform a full copy"
          dest_content_id ;
        None
    in
    try
      let remote_dp = Uuidx.(to_string (make ())) in
      let base_dp = Uuidx.(to_string (make ())) in
      let leaf_dp = Uuidx.(to_string (make ())) in
      let dest_vdi_url =
        let url' = Http.Url.of_string url in
        Http.Url.set_path url'
          (Printf.sprintf "%s/nbdproxy/import/%s/%s/%s/%s"
             (Http.Url.get_path url')
             (Storage_interface.Vm.string_of vm)
             (Storage_interface.Sr.string_of dest)
             (Storage_interface.Vdi.string_of dest_vdi)
             remote_dp
          )
        |> Http.Url.to_string
      in
      D.debug "%s copy remote NBD URL = %s" __FUNCTION__ dest_vdi_url ;
      let id = State.copy_id_of (sr, vdi) in
      D.debug "Persisting state for copy (id=%s)" id ;
      State.add id
        State.(
          Copy_op
            Copy_state.
              {
                base_dp
              ; leaf_dp
              ; remote_dp
              ; dest_sr= dest
              ; copy_vdi= remote_vdi.vdi
              ; remote_url= url
              ; verify_dest
              }
        ) ;
      SXM.info "%s: copy initiated local_vdi:%s dest_vdi:%s" __FUNCTION__
        (Storage_interface.Vdi.string_of vdi)
        (Storage_interface.Vdi.string_of dest_vdi) ;
      finally
        (fun () ->
          D.debug "activating RW datapath %s on remote" remote_dp ;
          let backend =
            Remote.VDI.attach3 dbg remote_dp dest dest_vdi vm true
          in
          let _, _, _, nbds =
            Storage_interface.implementations_of_backend backend
          in
          let proto =
            match nbds with
            | [] ->
                None
            | uri :: _ ->
                let _socket, export = Storage_interface.parse_nbd_uri uri in
                Some (`NBD export)
          in
          Remote.VDI.activate3 dbg remote_dp dest dest_vdi vm ;
          with_activated_disk ~dbg ~sr ~vdi:base_vdi ~dp:base_dp ~vm
            (fun base_path ->
              with_activated_disk ~dbg ~sr ~vdi:(Some vdi) ~dp:leaf_dp ~vm
                (fun src ->
                  let verify_cert =
                    if verify_dest then Stunnel_client.pool () else None
                  in
                  let dd =
                    Sparse_dd_wrapper.start
                      ~progress_cb:(progress_callback 0.05 0.9 task)
                      ~verify_cert ~proto ?base:base_path true (Option.get src)
                      dest_vdi_url remote_vdi.virtual_size
                  in
                  Storage_task.with_cancel task
                    (fun () -> Sparse_dd_wrapper.cancel dd)
                    (fun () ->
                      try Sparse_dd_wrapper.wait dd
                      with Sparse_dd_wrapper.Cancelled ->
                        Storage_task.raise_cancelled task
                    )
              )
          )
        )
        (fun () ->
          Remote.DP.destroy dbg remote_dp false ;
          State.remove_copy id
        ) ;
      SXM.info "%s: copy complete for local_vdi:%s dest_vdi:%s" __FUNCTION__
        (Storage_interface.Vdi.string_of vdi)
        (Storage_interface.Vdi.string_of dest_vdi) ;
      D.debug "setting remote content_id <- %s" local_vdi.content_id ;
      Remote.VDI.set_content_id dbg dest dest_vdi local_vdi.content_id ;
      (* PR-1255: XXX: this is useful because we don't have content_ids by default *)
      D.debug "setting local content_id <- %s" local_vdi.content_id ;
      Local.VDI.set_content_id dbg sr local_vdi.vdi local_vdi.content_id ;
      (* Re-find the VDI to get the updated content_id info *)
      let remote_vdi, _ =
        find_vdi ~dbg ~sr:dest ~vdi:dest_vdi (module Remote)
      in
      Some (Vdi_info remote_vdi)
    with e ->
      D.error "Caught %s: performing cleanup actions" (Printexc.to_string e) ;
      perform_cleanup_actions !on_fail ;
      raise e

  (** [copy_into_sr] does not requires a dest vdi to be provided, instead, it will 
  find the nearest vdi on the [dest] sr, and if there is no such vdi, it will 
  create one. *)
  let copy_into_sr ~task ~dbg ~sr ~vdi ~vm ~url ~dest ~verify_dest =
    D.debug "copy sr:%s vdi:%s url:%s dest:%s verify_dest:%B"
      (Storage_interface.Sr.string_of sr)
      (Storage_interface.Vdi.string_of vdi)
      url
      (Storage_interface.Sr.string_of dest)
      verify_dest ;
    let (module Remote) = get_remote_backend url verify_dest in
    (* Find the local VDI *)
    try
      let local_vdi, _ = find_vdi ~dbg ~sr ~vdi (module Local) in
      try
        let similar_vdis = Local.VDI.similar_content dbg sr vdi in
        let similars = List.map (fun vdi -> vdi.content_id) similar_vdis in
        D.debug "Similar VDIs = [ %s ]"
          (String.concat "; "
             (List.map
                (fun x ->
                  Printf.sprintf "(vdi=%s,content_id=%s)"
                    (Storage_interface.Vdi.string_of x.vdi)
                    x.content_id
                )
                similar_vdis
             )
          ) ;
        let remote_vdis = Remote.SR.scan dbg dest in
        (* We drop cbt_metadata VDIs that do not have any actual data *)
        let remote_vdis =
          List.filter (fun vdi -> vdi.ty <> "cbt_metadata") remote_vdis
        in
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
                         && vdi.virtual_size <= local_vdi.virtual_size
                       )
                       remote_vdis
                    )
                with Not_found -> None
              )
            )
            None similars
        in
        D.debug "Nearest VDI: content_id=%s vdi=%s"
          (Option.fold ~none:"None" ~some:(fun x -> x.content_id) nearest)
          (Option.fold ~none:"None"
             ~some:(fun x -> Storage_interface.Vdi.string_of x.vdi)
             nearest
          ) ;
        let remote_base =
          match nearest with
          | Some vdi ->
              D.debug "Cloning VDI" ;
              let vdi_clone = Remote.VDI.clone dbg dest vdi in
              D.debug "Clone: %s" (Storage_interface.Vdi.string_of vdi_clone.vdi) ;
              ( if vdi_clone.virtual_size <> local_vdi.virtual_size then
                  let new_size =
                    Remote.VDI.resize dbg dest vdi_clone.vdi
                      local_vdi.virtual_size
                  in
                  D.debug "Resize remote clone VDI to %Ld: result %Ld"
                    local_vdi.virtual_size new_size
              ) ;
              vdi_clone
          | None ->
              D.debug "Creating a blank remote VDI" ;
              Remote.VDI.create dbg dest {local_vdi with sm_config= []}
        in
        let remote_copy =
          copy_into_vdi ~task ~dbg ~sr ~vdi ~vm ~url ~dest
            ~dest_vdi:remote_base.vdi ~verify_dest
          |> vdi_info
        in
        let snapshot = Remote.VDI.snapshot dbg dest remote_copy in
        Remote.VDI.destroy dbg dest remote_copy.vdi ;
        Some (Vdi_info snapshot)
      with e ->
        D.error "Caught %s: copying snapshots vdi" (Printexc.to_string e) ;
        raise (Storage_error (Internal_error (Printexc.to_string e)))
    with
    | Storage_error (Backend_error (code, params))
    | Api_errors.Server_error (code, params) ->
        raise (Storage_error (Backend_error (code, params)))
    | e ->
        raise (Storage_error (Internal_error (Printexc.to_string e)))
end

let mirror_pass_fds ~dbg ~dp ~sr ~vdi ~mirror_vm ~live_vm ~mirror_id ~url
    ~dest_sr ~verify_dest ~(remote_mirror : Mirror.mirror_receive_result_vhd_t)
    =
  D.debug
    "%s dbg:%s dp:%s sr:%s vdi:%s mirror_vm:%s live_vm:%s mirror_id:%s url:%s \
     dest_sr:%s verify_dest:%B"
    __FUNCTION__ dbg dp (s_of_sr sr) (s_of_vdi vdi) (s_of_vm mirror_vm)
    (s_of_vm live_vm) mirror_id url (s_of_sr dest_sr) verify_dest ;
  let remote_vdi = remote_mirror.mirror_vdi.vdi in
  let mirror_dp = remote_mirror.mirror_datapath in

  let uri =
    Printf.sprintf "/services/SM/nbd/%s/%s/%s/%s"
      (Storage_interface.Vm.string_of mirror_vm)
      (Storage_interface.Sr.string_of dest_sr)
      (Storage_interface.Vdi.string_of remote_vdi)
      mirror_dp
  in
  D.debug "%s: uri of http request for mirroring is %s" __FUNCTION__ uri ;
  let dest_url = Http.Url.set_path (Http.Url.of_string url) uri in
  D.debug "%s url of http request for mirroring is %s" __FUNCTION__
    (Http.Url.to_string dest_url) ;
  let request =
    Http.Request.make
      ~query:(Http.Url.get_query_params dest_url)
      ~version:"1.0" ~user_agent:"smapiv2" Http.Put uri
  in
  let verify_cert = if verify_dest then Stunnel_client.pool () else None in
  let transport = Xmlrpc_client.transport_of_url ~verify_cert dest_url in
  D.debug "Searching for data path: %s" dp ;
  let attach_info = Local.DP.attach_info dbg sr vdi dp mirror_vm in

  let tapdev =
    match tapdisk_of_attach_info attach_info with
    | Some tapdev ->
        let pid = Tapctl.get_tapdisk_pid tapdev in
        let path = Printf.sprintf "/var/run/blktap-control/nbdclient%d" pid in
        with_transport ~stunnel_wait_disconnect:false transport
          (with_http request (fun (_response, s) ->
               (* Enable mirroring on the local machine *)
               let control_fd = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
               finally
                 (fun () ->
                   Unix.connect control_fd (Unix.ADDR_UNIX path) ;
                   let msg = dp in
                   let len = String.length msg in
                   let written =
                     Unixext.send_fd_substring control_fd msg 0 len [] s
                   in
                   if written <> len then (
                     D.error "Failed to transfer fd to %s" path ;
                     failwith "Internal error transferring fd to tapdisk"
                   )
                 )
                 (fun () -> Unix.close control_fd)
           )
          ) ;
        tapdev
    | None ->
        D.error "%s: vdi %s not attached" __FUNCTION__ (Vdi.string_of vdi) ;
        raise
          (Storage_interface.Storage_error
             (Migration_mirror_fd_failure "VDI Not Attached")
          )
    | exception e ->
        D.error "%s Caught exception %s:. Performing cleanup." __FUNCTION__
          (Printexc.to_string e) ;
        raise
          (Storage_interface.Storage_error
             (Migration_mirror_fd_failure (Printexc.to_string e))
          )
  in
  D.debug "%s Updating active local mirrors: id=%s" __FUNCTION__ mirror_id ;
  let alm =
    State.Send_state.
      {
        url
      ; dest_sr
      ; remote_info=
          Some
            {
              dp= remote_mirror.mirror_datapath
            ; vdi= remote_mirror.mirror_vdi.vdi
            ; url
            ; verify_dest
            }
      ; local_dp= dp
      ; tapdev= Some tapdev
      ; failed= false
      ; watchdog= None
      ; vdi
      ; live_vm
      ; mirror_key= None
      }
  in
  State.add mirror_id (State.Send_op alm) ;
  D.debug "%s Updated mirror_id %s in the active local mirror" __FUNCTION__
    mirror_id ;
  tapdev

let mirror_snapshot ~dbg ~sr ~dp ~mirror_id ~local_vdi =
  D.debug "%s dbg:%s sr:%s dp:%s mirror_id:%s local_vdi:%s" __FUNCTION__ dbg
    (s_of_sr sr) dp mirror_id
    (string_of_vdi_info local_vdi) ;
  SXM.info "%s About to snapshot VDI = %s" __FUNCTION__
    (string_of_vdi_info local_vdi) ;
  let local_vdi = add_to_sm_config local_vdi "mirror" ("nbd:" ^ dp) in
  let local_vdi = add_to_sm_config local_vdi "base_mirror" mirror_id in
  let snapshot =
    try Local.VDI.snapshot dbg sr local_vdi with
    | Storage_interface.Storage_error (Backend_error (code, _))
      when code = "SR_BACKEND_FAILURE_44" ->
        raise
          (Storage_interface.Storage_error
             (Migration_mirror_snapshot_failure
                (Printf.sprintf "%s:%s" Api_errors.sr_source_space_insufficient
                   (Storage_interface.Sr.string_of sr)
                )
             )
          )
    | e ->
        raise
          (Storage_interface.Storage_error
             (Migration_mirror_snapshot_failure (Printexc.to_string e))
          )
  in

  SXM.info "%s: snapshot created, mirror initiated vdi:%s snapshot_of:%s"
    __FUNCTION__
    (Storage_interface.Vdi.string_of snapshot.vdi)
    (Storage_interface.Vdi.string_of local_vdi.vdi) ;

  snapshot

let mirror_checker mirror_id tapdev =
  let rec inner () =
    let alm_opt = State.find_active_local_mirror mirror_id in
    match alm_opt with
    | Some alm ->
        let stats = Tapctl.stats (Tapctl.create ()) tapdev in
        if stats.Tapctl.Stats.nbd_mirror_failed = 1 then (
          D.error "Tapdisk mirroring has failed" ;
          Updates.add (Dynamic.Mirror mirror_id) updates
        ) ;
        alm.State.Send_state.watchdog <-
          Some
            (Scheduler.one_shot scheduler (Scheduler.Delta 5) "tapdisk_watchdog"
               inner
            )
    | None ->
        ()
  in
  inner ()

let mirror_copy ~task ~dbg ~sr ~snapshot ~copy_vm ~url ~dest_sr ~remote_mirror
    ~verify_dest =
  (* Copy the snapshot to the remote *)
  try
    Storage_task.with_subtask task "copy" (fun () ->
        Copy.copy_into_vdi ~task ~dbg ~sr ~vdi:snapshot.vdi ~vm:copy_vm ~url
          ~dest:dest_sr ~dest_vdi:remote_mirror.Mirror.copy_diffs_to
          ~verify_dest
    )
    |> vdi_info
  with e ->
    raise (Storage_error (Migration_mirror_copy_failure (Printexc.to_string e)))

let mirror_cleanup ~dbg ~sr ~snapshot =
  D.debug "Destroying snapshot on src" ;
  Local.VDI.destroy dbg sr snapshot.vdi

module MIRROR : SMAPIv2_MIRROR = struct
  type context = unit

  let send_start _ctx ~dbg ~task_id ~dp ~sr ~vdi ~mirror_vm ~mirror_id
      ~local_vdi ~copy_vm ~live_vm ~url ~remote_mirror ~dest_sr ~verify_dest =
    D.debug
      "%s dbg: %s dp: %s sr: %s vdi:%s mirror_vm:%s mirror_id: %s live_vm: %s \
       url:%s dest_sr:%s verify_dest:%B"
      __FUNCTION__ dbg dp (s_of_sr sr) (s_of_vdi vdi) (s_of_vm mirror_vm)
      mirror_id (s_of_vm live_vm) url (s_of_sr dest_sr) verify_dest ;
    let (module Remote) =
      Storage_migrate_helper.get_remote_backend url verify_dest
    in

    let read_write = true in
    (* DP set up is only essential for MIRROR.start/stop due to their open ended pattern.
       It's not necessary for copy which will take care of that itself. *)
    ignore (Local.VDI.attach3 dbg dp sr vdi (Vm.of_string "0") read_write) ;
    Local.VDI.activate3 dbg dp sr vdi (Vm.of_string "0") ;
    match remote_mirror with
    | Mirror.SMAPIv3_mirror _ ->
        (* this should never happen *)
        raise
          (Storage_error
             (Migration_mirror_failure
                "Incorrect remote mirror format for SMAPIv1"
             )
          )
    | Mirror.Vhd_mirror mirror_res ->
        let tapdev =
          mirror_pass_fds ~dbg ~dp ~sr ~vdi ~mirror_vm ~live_vm ~mirror_id ~url
            ~dest_sr ~verify_dest ~remote_mirror:mirror_res
        in

        let snapshot = mirror_snapshot ~dbg ~sr ~dp ~mirror_id ~local_vdi in

        mirror_checker mirror_id tapdev ;
        let task = Storage_task.(handle_of_id tasks) task_id in
        let new_parent =
          mirror_copy ~task ~dbg ~sr ~snapshot ~copy_vm ~url ~dest_sr
            ~remote_mirror:mirror_res ~verify_dest
        in

        D.debug "Local VDI %s = remote VDI %s"
          (Storage_interface.Vdi.string_of snapshot.vdi)
          (Storage_interface.Vdi.string_of new_parent.vdi) ;
        D.debug "Local VDI %s now mirrored to remote VDI: %s"
          (Storage_interface.Vdi.string_of local_vdi.vdi)
          (Storage_interface.Vdi.string_of mirror_res.Mirror.mirror_vdi.vdi) ;
        mirror_cleanup ~dbg ~sr ~snapshot

  let receive_start_common ~dbg ~sr ~vdi_info ~id ~similar ~vm
      (module SMAPI : SMAPIv2) =
    let on_fail : (unit -> unit) list ref = ref [] in
    let vdis = SMAPI.SR.scan dbg sr in
    (* We drop cbt_metadata VDIs that do not have any actual data *)
    let vdis = List.filter (fun vdi -> vdi.ty <> "cbt_metadata") vdis in
    let leaf_dp = SMAPI.DP.create dbg Uuidx.(to_string (make ())) in
    try
      let vdi_info = {vdi_info with sm_config= [("base_mirror", id)]} in
      let leaf = SMAPI.VDI.create dbg sr vdi_info in
      D.info "Created leaf VDI for mirror receive: %s" (string_of_vdi_info leaf) ;
      on_fail := (fun () -> SMAPI.VDI.destroy dbg sr leaf.vdi) :: !on_fail ;
      (* dummy VDI is created so that the leaf VDI becomes a differencing disk,
         useful for calling VDI.compose later on *)
      let dummy = SMAPI.VDI.snapshot dbg sr leaf in
      on_fail := (fun () -> SMAPI.VDI.destroy dbg sr dummy.vdi) :: !on_fail ;
      D.debug "%s Created dummy snapshot for mirror receive: %s" __FUNCTION__
        (string_of_vdi_info dummy) ;
      let _ : backend = SMAPI.VDI.attach3 dbg leaf_dp sr leaf.vdi vm true in
      SMAPI.VDI.activate3 dbg leaf_dp sr leaf.vdi vm ;
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
      D.debug "Nearest VDI: content_id=%s vdi=%s"
        (Option.fold ~none:"None" ~some:(fun x -> x.content_id) nearest)
        (Option.fold ~none:"None"
           ~some:(fun x -> Storage_interface.Vdi.string_of x.vdi)
           nearest
        ) ;
      let parent =
        match nearest with
        | Some vdi ->
            D.debug "Cloning VDI" ;
            let vdi = add_to_sm_config vdi "base_mirror" id in
            let vdi_clone = SMAPI.VDI.clone dbg sr vdi in
            D.debug "Clone: %s" (Storage_interface.Vdi.string_of vdi_clone.vdi) ;
            ( if vdi_clone.virtual_size <> vdi_info.virtual_size then
                let new_size =
                  SMAPI.VDI.resize dbg sr vdi_clone.vdi vdi_info.virtual_size
                in
                D.debug "Resize clone VDI to %Ld: result %Ld"
                  vdi_info.virtual_size new_size
            ) ;
            vdi_clone
        | None ->
            D.debug "Creating a blank remote VDI" ;
            SMAPI.VDI.create dbg sr vdi_info
      in
      D.debug "Parent disk content_id=%s" parent.content_id ;
      (* The state tracking here does not need to be changed, however, it will be
         stored in memory on different hosts. If receive_start is called, by an older
         host, this State.add is run on the destination host. On the other hand, if
         receive_start3 is called, this will be stored in memory on the source host.
         receive_finalize3 and receive_cancel2 handles this similarly. *)
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
              ; url= ""
              ; verify_dest= false
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
            D.debug "Caught exception in on_fail: %s" (Printexc.to_string e)
        )
        !on_fail ;
      raise e

  let receive_start _ctx ~dbg ~sr ~vdi_info ~id ~similar =
    D.debug "%s dbg: %s sr: %s vdi: %s id: %s" __FUNCTION__ dbg (s_of_sr sr)
      (string_of_vdi_info vdi_info)
      id ;
    receive_start_common ~dbg ~sr ~vdi_info ~id ~similar ~vm:(Vm.of_string "0")
      (module Local)

  let receive_start2 _ctx ~dbg ~sr ~vdi_info ~id ~similar ~vm =
    D.debug "%s dbg: %s sr: %s vdi: %s id: %s" __FUNCTION__ dbg (s_of_sr sr)
      (string_of_vdi_info vdi_info)
      id ;
    receive_start_common ~dbg ~sr ~vdi_info ~id ~similar ~vm (module Local)

  let receive_start3 _ctx ~dbg ~sr ~vdi_info ~mirror_id ~similar ~vm ~url
      ~verify_dest =
    D.debug "%s dbg: %s sr: %s vdi: %s id: %s vm: %s url: %s verify_dest: %B"
      __FUNCTION__ dbg (s_of_sr sr)
      (string_of_vdi_info vdi_info)
      mirror_id (s_of_vm vm) url verify_dest ;
    let (module Remote) =
      Storage_migrate_helper.get_remote_backend url verify_dest
    in
    receive_start_common ~dbg ~sr ~vdi_info ~id:mirror_id ~similar ~vm
      (module Remote)

  let receive_finalize _ctx ~dbg ~id =
    D.debug "%s dbg:%s id: %s" __FUNCTION__ dbg id ;
    let recv_state = State.find_active_receive_mirror id in
    let open State.Receive_state in
    Option.iter (fun r -> Local.DP.destroy dbg r.leaf_dp false) recv_state ;
    State.remove_receive_mirror id

  let receive_finalize_common ~dbg ~mirror_id (module SMAPI : SMAPIv2) =
    let recv_state = State.find_active_receive_mirror mirror_id in
    let open State.Receive_state in
    Option.iter
      (fun r ->
        SXM.info
          "%s Mirror done. Compose on the dest sr %s parent %s and leaf %s"
          __FUNCTION__ (Sr.string_of r.sr)
          (Vdi.string_of r.parent_vdi)
          (Vdi.string_of r.leaf_vdi) ;
        SMAPI.DP.destroy2 dbg r.leaf_dp r.sr r.leaf_vdi r.mirror_vm false ;
        SMAPI.VDI.compose dbg r.sr r.parent_vdi r.leaf_vdi ;
        (* On SMAPIv3, compose would have removed the now invalid dummy vdi, so
           there is no need to destroy it anymore, while this is necessary on SMAPIv1 SRs. *)
        D.log_and_ignore_exn (fun () -> SMAPI.VDI.destroy dbg r.sr r.dummy_vdi) ;
        SMAPI.VDI.remove_from_sm_config dbg r.sr r.leaf_vdi "base_mirror"
      )
      recv_state ;
    State.remove_receive_mirror mirror_id

  let receive_finalize2 _ctx ~dbg ~id =
    D.debug "%s dbg:%s id: %s" __FUNCTION__ dbg id ;
    receive_finalize_common ~dbg ~mirror_id:id (module Local)

  let receive_finalize3 _ctx ~dbg ~mirror_id ~sr ~url ~verify_dest =
    D.debug "%s dbg:%s id: %s sr: %s url: %s verify_dest: %B" __FUNCTION__ dbg
      mirror_id (s_of_sr sr) url verify_dest ;
    let (module Remote) =
      Storage_migrate_helper.get_remote_backend url verify_dest
    in
    receive_finalize_common ~dbg ~mirror_id (module Remote)

  let receive_cancel _ctx ~dbg ~id =
    D.debug "%s dbg:%s mirror_id:%s" __FUNCTION__ dbg id ;
    let receive_state = State.find_active_receive_mirror id in
    let open State.Receive_state in
    Option.iter
      (fun r ->
        D.log_and_ignore_exn (fun () -> Local.DP.destroy dbg r.leaf_dp false) ;
        List.iter
          (fun v ->
            D.log_and_ignore_exn (fun () -> Local.VDI.destroy dbg r.sr v)
          )
          [r.dummy_vdi; r.leaf_vdi; r.parent_vdi]
      )
      receive_state ;
    State.remove_receive_mirror id

  exception Timeout of Mtime.Span.t

  let reqs_outstanding_timeout = Mtime.Span.(150 * s)

  let pp_time () = Fmt.str "%a" Mtime.Span.pp

  (* Tapdisk should time out after 2 mins. We can wait a little longer *)

  let pre_deactivate_hook _ctx ~dbg ~dp ~sr ~vdi =
    D.debug "%s dbg:%s dp:%s sr:%s vdi:%s" __FUNCTION__ dbg dp (s_of_sr sr)
      (s_of_vdi vdi) ;
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
                   if Mtime.Span.compare elapsed reqs_outstanding_timeout > 0
                   then
                     raise (Timeout elapsed) ;
                   let st = stats ctx tapdev in
                   if st.Stats.reqs_outstanding > 0 then (
                     Thread.delay 1.0 ; wait ()
                   ) else
                     (st, elapsed)
                 in
                 let st, elapsed = wait () in
                 D.debug "Got final stats after waiting %a" pp_time elapsed ;
                 if st.Stats.nbd_mirror_failed = 1 then (
                   D.error "tapdisk reports mirroring failed" ;
                   s.failed <- true
                 ) ;
                 Option.iter
                   (fun id -> Scheduler.cancel scheduler id)
                   s.watchdog
           with
           | Timeout elapsed ->
               D.error
                 "Timeout out after %a waiting for tapdisk to complete all \
                  outstanding requests while migrating vdi %s of domain %s"
                 pp_time elapsed (s_of_vdi vdi) (s_of_vm s.live_vm) ;
               s.failed <- true
           | e ->
               D.error
                 "Caught exception while finally checking mirror state: %s \
                  when migrating vdi %s of domain %s"
                 (Printexc.to_string e) (s_of_vdi vdi) (s_of_vm s.live_vm) ;
               s.failed <- true
       )

  let has_mirror_failed _ctx ~dbg:_ ~mirror_id ~sr:_ =
    match State.find_active_local_mirror mirror_id with
    | Some {tapdev= Some tapdev; failed; _} -> (
      try
        let stats = Tapctl.stats (Tapctl.create ()) tapdev in
        stats.Tapctl.Stats.nbd_mirror_failed = 1
      with _ ->
        D.debug "Using cached copy of failure status" ;
        failed
    )
    | _ ->
        false

  let list _ctx = Storage_interface.unimplemented __FUNCTION__

  let stat _ctx = Storage_interface.unimplemented __FUNCTION__

  let receive_cancel2 _ctx ~dbg ~mirror_id ~url ~verify_dest =
    let (module Remote) =
      Storage_migrate_helper.get_remote_backend url verify_dest
    in
    let receive_state = State.find_active_receive_mirror mirror_id in
    let open State.Receive_state in
    Option.iter
      (fun r ->
        D.log_and_ignore_exn (fun () -> Remote.DP.destroy dbg r.leaf_dp false) ;
        List.iter
          (fun v ->
            D.log_and_ignore_exn (fun () -> Remote.VDI.destroy dbg r.sr v)
          )
          [r.dummy_vdi; r.leaf_vdi; r.parent_vdi]
      )
      receive_state ;
    State.remove_receive_mirror mirror_id
end
