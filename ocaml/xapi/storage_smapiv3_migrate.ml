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

module Date = Clock.Date
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

(** Path of the unix-domain socket on which the local NBD proxy thread listens
    for connections from QEMU on behalf of [mirror_vm]. The same path is used
    by the proxy server (export side) and by the URI passed to the destination
    (import side), so derive both from this single helper. *)
let nbd_proxy_path_of_vm vm =
  Printf.sprintf "/var/run/nbdproxy/export/%s" (Vm.string_of vm)

let export_nbd_proxy ~remote_url ~mirror_vm ~sr ~vdi ~dp ~verify_dest =
  D.debug "%s spawning exporting nbd proxy" __FUNCTION__ ;
  let path = nbd_proxy_path_of_vm mirror_vm in
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

let mirror_poll_interval = 0.5

(** Poll the mirror until complete; on failure raise Migration_mirror_failure
    and, when [mirror_id] is given, mark Send_state.failed and post an update. *)
let wait_for_mirror ~dbg ~sr ~vdi ~vm ?mirror_id ~error_msg mirror_key =
  let on_failure () =
    match mirror_id with
    | Some mid ->
        State.find_active_local_mirror mid
        |> Option.iter (fun (s : State.Send_state.t) -> s.failed <- true) ;
        Updates.add (Dynamic.Mirror mid) updates
    | None ->
        ()
  in
  let rec poll key =
    let {failed; complete; progress} : Mirror.status =
      Local.DATA.stat dbg sr vdi vm key
    in
    if complete then
      Option.iter
        (fun p -> D.debug "%s mirror completed, progress: %.2f" __FUNCTION__ p)
        progress
    else if failed then (
      on_failure () ;
      raise
        (Storage_interface.Storage_error (Migration_mirror_failure error_msg))
    ) else (
      Option.iter
        (fun p -> D.debug "%s mirror progress: %.2f" __FUNCTION__ p)
        progress ;
      Unix.sleepf mirror_poll_interval ;
      poll key
    )
  in
  match mirror_key with
  | Storage_interface.Mirror.CopyV1 _ ->
      ()
  | Storage_interface.Mirror.MirrorV1 _ ->
      D.debug "%s waiting for mirror to complete" __FUNCTION__ ;
      poll mirror_key

(** Deactivate and detach a snapshot VDI. Uses [Fun.protect] so [detach]
    runs even if [deactivate] raises -- otherwise the datapath leaks until
    reboot (xapi has no datapath GC). *)
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

let mirror_snapshot_into_existing_dest ~dbg ~sr ~snapshot_vdi_uuid ~dest_sr
    ~dest_url ~verify_dest ~copy_vm ~image_format ~dest_vdi_info ~nbd_uri =
  SXM.info "%s mirroring snapshot %s into VDI %s" __FUNCTION__ snapshot_vdi_uuid
    (s_of_vdi dest_vdi_info.vdi) ;

  let snapshot_vdi = Vdi.of_string snapshot_vdi_uuid in
  let dp = Uuidx.(to_string (make ())) in

  try
    (* Capture src snapshot content_id BEFORE attaching, so we can
       propagate it onto the dest snapshot to satisfy
       assert_content_ids_match in update_snapshot_info_dest. *)
    let src_content_id =
      try (Local.VDI.stat dbg sr snapshot_vdi).content_id
      with e ->
        D.warn "%s failed to stat src snapshot %s for content_id: %s"
          __FUNCTION__ snapshot_vdi_uuid (Printexc.to_string e) ;
        ""
    in
    D.debug "%s captured src snapshot %s content_id=%s" __FUNCTION__
      snapshot_vdi_uuid src_content_id ;

    ignore (Local.VDI.attach3 dbg dp sr snapshot_vdi copy_vm false) ;
    Local.VDI.activate_readonly dbg dp sr snapshot_vdi copy_vm ;

    D.debug "%s starting QEMU mirror from snapshot %s" __FUNCTION__
      snapshot_vdi_uuid ;
    let mirror_key =
      Local.DATA.mirror dbg sr snapshot_vdi image_format copy_vm nbd_uri
    in

    wait_for_mirror ~dbg ~sr ~vdi:snapshot_vdi ~vm:copy_vm
      ~error_msg:(Printf.sprintf "Snapshot %s mirror failed" snapshot_vdi_uuid)
      mirror_key ;

    detach_snapshot_vdi ~dbg ~dp ~sr ~snapshot_vdi ~copy_vm ;

    let dest_snapshot =
      create_destination_snapshot ~dbg ~dest_sr ~dest_url ~verify_dest
        ~dest_vdi_info ~src_content_id
    in
    D.debug "%s destination snapshot created: %s" __FUNCTION__
      (s_of_vdi dest_snapshot.vdi) ;

    dest_snapshot
  with e ->
    D.error "%s snapshot mirror failed: %s" __FUNCTION__ (Printexc.to_string e) ;
    D.log_and_ignore_exn (fun () ->
        detach_snapshot_vdi ~dbg ~dp ~sr ~snapshot_vdi ~copy_vm
    ) ;
    raise e

let nbd_export_of_attach_info backend =
  match Storage_interface.nbd_export_of_attach_info backend with
  | Some export ->
      export
  | None ->
      raise
        (Storage_error
           (Migration_preparation_failure "No NBD export found in attach info")
        )

(** A node in the VM snapshot tree, projected onto a single disk position
    (identified by VDI lineage — see [find_lineage_vdi_on_snapshot_vm]).
    Branching at a node means the source VM was reverted to that snapshot
    before further snapshots were taken; among the children exactly one
    lies on the active path (the chain leading to the live VM) and the rest
    are reverted-and-orphaned branches that still need to be reproduced on
    the destination so the snapshot tree topology survives migration. *)
type snapshot_tree_node = {
    vdi_uuid: string
  ; snapshot_time: Date.t
  ; on_active_path: bool
  ; children: snapshot_tree_node list
}

let find_active_vm_for_vdi ~__context ~vdi_ref =
  Db.VDI.get_VBDs ~__context ~self:vdi_ref
  |> List.find_map (fun vbd ->
      let vm = Db.VBD.get_VM ~__context ~self:vbd in
      if Db.VM.get_is_a_snapshot ~__context ~self:vm then
        None
      else
        Some vm
  )

(** For a snapshot VM, find the disk VDI that belongs to [lineage] (the set
    of snapshot VDI refs of the disk being migrated). *)
let find_lineage_vdi_on_snapshot_vm ~__context ~lineage ~snapshot_vm =
  Db.VM.get_VBDs ~__context ~self:snapshot_vm
  |> List.find_map (fun vbd ->
      if Db.VBD.get_type ~__context ~self:vbd <> `Disk then
        None
      else
        let vdi = Db.VBD.get_VDI ~__context ~self:vbd in
        if not (List.mem vdi lineage) then
          None
        else
          let uuid = Db.VDI.get_uuid ~__context ~self:vdi in
          let time = Db.VDI.get_snapshot_time ~__context ~self:vdi in
          Some (uuid, time)
  )

let sort_snapshots_by_time ~__context snaps =
  List.sort
    (fun a b ->
      Date.compare
        (Db.VM.get_snapshot_time ~__context ~self:a)
        (Db.VM.get_snapshot_time ~__context ~self:b)
    )
    snaps

(** Walk the [parent] chain upward from [vm] to collect the set of
    snapshot VMs on the active path (the chronological line from the
    oldest root snapshot down to [vm]). The result is a set membership
    predicate, returned as a [Ref.t -> bool]. *)
let build_active_path_predicate ~__context ~vm =
  let rec collect snap acc =
    if snap = Ref.null then
      acc
    else
      collect (Db.VM.get_parent ~__context ~self:snap) (snap :: acc)
  in
  let path = collect (Db.VM.get_parent ~__context ~self:vm) [] in
  fun snap -> List.mem snap path

(** Build the snapshot subtree rooted at [snapshot_vm], projected onto a
    single disk identified by [lineage] (the disk's snapshot VDI set). A
    node whose snapshot VM has no VDI in [lineage] is skipped: its
    descendants (if any match) are promoted to take its place, so we never
    lose snapshots that exist for the disk just because an intermediate
    snapshot VM did not include it. *)
let rec build_subtree_for_lineage ~__context ~lineage ~on_active_path
    snapshot_vm =
  let recur_children () =
    Db.VM.get_children ~__context ~self:snapshot_vm
    |> List.filter (fun c -> Db.VM.get_is_a_snapshot ~__context ~self:c)
    |> sort_snapshots_by_time ~__context
    |> List.concat_map
         (build_subtree_for_lineage ~__context ~lineage ~on_active_path)
  in
  match find_lineage_vdi_on_snapshot_vm ~__context ~lineage ~snapshot_vm with
  | Some (vdi_uuid, snapshot_time) ->
      [
        {
          vdi_uuid
        ; snapshot_time
        ; on_active_path= on_active_path snapshot_vm
        ; children= recur_children ()
        }
      ]
  | None ->
      (* Promote descendants: a snapshot VM without this disk is a
         transparent intermediate — its disk-bearing descendants must
         still appear in the tree at the parent's level. *)
      recur_children ()

(** Build the forest of snapshot-tree nodes for the disk identified by
    [lineage], walking the snapshot VMs of [vm] from the roots of the VM
    snapshot tree (those with no [parent]) and projecting each onto
    [lineage]. [on_active_path] decides which snapshot VMs lie on the chain
    leading to the live VM. *)
let build_forest_for_lineage ~__context ~vm ~lineage ~on_active_path =
  Db.VM.get_snapshots ~__context ~self:vm
  |> List.filter (fun s -> Db.VM.get_parent ~__context ~self:s = Ref.null)
  |> sort_snapshots_by_time ~__context
  |> List.concat_map
       (build_subtree_for_lineage ~__context ~lineage ~on_active_path)

let rec count_tree_nodes trees =
  List.fold_left
    (fun acc node -> acc + 1 + count_tree_nodes node.children)
    0 trees

(** Retrieve the snapshot tree for the active disk [vdi] being migrated.
    The tree is built by:
      1. locating the live VM that has [vdi] attached,
      2. taking the disk's lineage = [VDI.snapshots] of [vdi] (the correct,
         revert- and slot-reuse-safe snapshot-to-disk identity),
      3. walking the live VM's snapshot tree and projecting each snapshot VM
         onto the lineage.
    Returns root nodes sorted by [snapshot_time] (oldest first). *)
let get_snapshot_tree ~dbg ~vdi =
  D.debug "%s retrieving snapshot tree for VDI %s" __FUNCTION__ (s_of_vdi vdi) ;
  try
    Server_helpers.exec_with_new_task "get_snapshot_tree"
      ~subtask_of:(Ref.of_string dbg) (fun __context ->
        let vdi_uuid = s_of_vdi vdi in
        let vdi_ref = Db.VDI.get_by_uuid ~__context ~uuid:vdi_uuid in
        match find_active_vm_for_vdi ~__context ~vdi_ref with
        | None ->
            D.warn "%s no active VM found for VDI %s" __FUNCTION__ vdi_uuid ;
            []
        | Some vm ->
            let lineage = Db.VDI.get_snapshots ~__context ~self:vdi_ref in
            if lineage = [] then (
              D.debug "%s no snapshots for VDI %s" __FUNCTION__ vdi_uuid ;
              []
            ) else
              let on_active_path = build_active_path_predicate ~__context ~vm in
              let roots =
                build_forest_for_lineage ~__context ~vm ~lineage ~on_active_path
              in
              D.debug "%s VDI %s lineage of %d, %d root(s), %d node(s)"
                __FUNCTION__ vdi_uuid (List.length lineage) (List.length roots)
                (count_tree_nodes roots) ;
              roots
    )
  with e ->
    D.error "%s failed to retrieve snapshot tree: %s" __FUNCTION__
      (Printexc.to_string e) ;
    raise
      (Storage_error
         (Migration_preparation_failure
            (Printf.sprintf "Failed to build snapshot tree for VDI %s: %s"
               (s_of_vdi vdi) (Printexc.to_string e)
            )
         )
      )

let start_nbd_proxy_thread ~url ~mirror_vm ~dest_sr ~mirror_vdi ~mirror_datapath
    ~verify_dest =
  Thread.create
    (fun () ->
      export_nbd_proxy ~remote_url:url ~mirror_vm ~sr:dest_sr
        ~vdi:mirror_vdi.vdi ~dp:mirror_datapath ~verify_dest
    )
    ()

let switch_vdi ~mode ~dbg ~url ~verify_dest ~mirror_datapath ~dest_sr
    ~mirror_vdi ~mirror_vm =
  let (module Remote) =
    Storage_migrate_helper.get_remote_backend url verify_dest
  in
  let activate =
    match mode with
    | `Readonly ->
        Remote.VDI.activate_readonly
    | `Writable ->
        Remote.VDI.activate3
  in
  Remote.VDI.deactivate dbg mirror_datapath dest_sr mirror_vdi.vdi mirror_vm ;
  activate dbg mirror_datapath dest_sr mirror_vdi.vdi mirror_vm

(** Invariant context shared across all nodes in a snapshot tree traversal.
    Extracted to avoid threading 8+ parameters through every recursive call.
    [on_fail] accumulates undo thunks (most recently registered first) that
    [send_start] runs in reverse if any node's mirror fails, so a partial
    DFS does not leak per-node destination snapshot VDIs. *)
type mirror_ctx = {
    dbg: string
  ; sr: Sr.t
  ; dest_sr: Sr.t
  ; url: string
  ; verify_dest: bool
  ; mirror_vm: Vm.t
  ; copy_vm: Vm.t
  ; nbd_proxy_path: string
  ; image_format: string
  ; on_fail: (unit -> unit) list ref
}

let nbd_uri_of_export ~nbd_proxy_path export =
  Uri.make ~scheme:"nbd+unix" ~host:"" ~path:export
    ~query:[("socket", [nbd_proxy_path])]
    ()
  |> Uri.to_string

let prepare_branch_vdi ~ctx ~dest_snapshot =
  let (module Remote) =
    Storage_migrate_helper.get_remote_backend ctx.url ctx.verify_dest
  in
  let bv = Remote.VDI.clone ctx.dbg ctx.dest_sr dest_snapshot in
  let bdp = Uuidx.(to_string (make ())) in
  D.debug "%s branch VDI %s cloned from snapshot %s" __FUNCTION__
    (s_of_vdi bv.vdi)
    (s_of_vdi dest_snapshot.vdi) ;
  let backend =
    Remote.VDI.attach3 ctx.dbg bdp ctx.dest_sr bv.vdi ctx.mirror_vm true
  in
  Remote.VDI.activate_readonly ctx.dbg bdp ctx.dest_sr bv.vdi ctx.mirror_vm ;
  let nbd_uri =
    nbd_uri_of_export ~nbd_proxy_path:ctx.nbd_proxy_path
      (nbd_export_of_attach_info backend)
  in
  let cleanup () =
    D.debug "%s cleaning up branch VDI %s" __FUNCTION__ (s_of_vdi bv.vdi) ;
    D.log_and_ignore_exn (fun () ->
        Remote.VDI.deactivate ctx.dbg bdp ctx.dest_sr bv.vdi ctx.mirror_vm
    ) ;
    D.log_and_ignore_exn (fun () ->
        Remote.VDI.detach ctx.dbg bdp ctx.dest_sr bv.vdi ctx.mirror_vm
    ) ;
    D.log_and_ignore_exn (fun () ->
        Remote.VDI.destroy ctx.dbg ctx.dest_sr bv.vdi
    )
  in
  (bv, bdp, nbd_uri, cleanup)

(** Mirror a single snapshot-tree node into the supplied [working_vdi],
    take a destination snapshot to anchor the mirrored data, and return
    the destination snapshot info plus the [snapshot_relation] entry.

    This is the unit of work executed at every node, regardless of
    whether the node sits on the active path or on a reverted branch --
    the only thing that changes between those cases is which VDI is
    passed as [working_vdi]. *)
let mirror_node_into ~ctx ~working_vdi ~working_dp ~nbd_uri ~counter ~total node
    =
  incr counter ;
  SXM.info "%s [%d/%d] mirroring snapshot %s into VDI %s" __FUNCTION__ !counter
    total node.vdi_uuid (s_of_vdi working_vdi.vdi) ;
  (* Give the NBD proxy thread time to start listening before issuing
     the mirror RPC. A proper fix would use a Mutex+Condition for
     readiness signalling -- tracked as a follow-up. *)
  let _ : Thread.t =
    start_nbd_proxy_thread ~url:ctx.url ~mirror_vm:ctx.mirror_vm
      ~dest_sr:ctx.dest_sr ~mirror_vdi:working_vdi ~mirror_datapath:working_dp
      ~verify_dest:ctx.verify_dest
  in
  Unix.sleepf mirror_poll_interval ;
  let dest_snapshot =
    mirror_snapshot_into_existing_dest ~dbg:ctx.dbg ~sr:ctx.sr
      ~snapshot_vdi_uuid:node.vdi_uuid ~dest_sr:ctx.dest_sr ~dest_url:ctx.url
      ~verify_dest:ctx.verify_dest ~copy_vm:ctx.copy_vm
      ~image_format:ctx.image_format ~dest_vdi_info:working_vdi ~nbd_uri
  in
  let (module Remote) =
    Storage_migrate_helper.get_remote_backend ctx.url ctx.verify_dest
  in
  ctx.on_fail :=
    (fun () -> Remote.VDI.destroy ctx.dbg ctx.dest_sr dest_snapshot.vdi)
    :: !(ctx.on_fail) ;
  let relation =
    State.
      {
        src_vdi= Vdi.of_string node.vdi_uuid
      ; dest_vdi= dest_snapshot.vdi
      ; snapshot_time= node.snapshot_time
      }
  in
  (dest_snapshot, relation)

(** DFS through the snapshot tree.

    Invariants:
    - [working_vdi] is the destination VDI currently impersonating the
      source disk's leaf as we walk down a chain. On the root of the
      whole tree this is the [mirror_vdi] created by [receive_start3];
      whenever we descend into a non-active sibling we clone the parent
      snapshot to obtain a fresh [working_vdi] for that subtree.
    - At a branching node (multiple children) we process the
      reverted/inactive subtrees first, each with its own cloned
      [working_vdi] which is destroyed as soon as the subtree finishes.
      The single active-path child is processed last and inherits the
      same [working_vdi], so when recursion bottoms out on the deepest
      active node the [working_vdi] is still the original [mirror_vdi]
      and is correctly positioned for live-leaf mirroring.
    - Inactive-first ordering also keeps resource usage low: a cloned
      branch's attach handle, NBD socket and qemu mirror job are torn
      down before the next branch starts. *)
let rec dfs_process_node ~ctx ~working_vdi ~working_dp ~nbd_uri ~counter ~total
    node =
  let dest_snapshot, this_relation =
    mirror_node_into ~ctx ~working_vdi ~working_dp ~nbd_uri ~counter ~total node
  in
  let inactive_children, active_children =
    List.partition (fun c -> not c.on_active_path) node.children
  in
  if List.length active_children > 1 then
    D.warn "%s node %s has %d active-path children, expected <= 1" __FUNCTION__
      node.vdi_uuid
      (List.length active_children) ;
  (* Process reverted/inactive subtrees first: each is rooted on a
     fresh clone of [dest_snapshot] and its working VDI is destroyed
     after the subtree finishes. *)
  let inactive_relations =
    List.concat_map
      (fun child ->
        let branch_vdi, branch_dp, branch_nbd_uri, cleanup =
          prepare_branch_vdi ~ctx ~dest_snapshot
        in
        Fun.protect ~finally:cleanup (fun () ->
            dfs_process_node ~ctx ~working_vdi:branch_vdi ~working_dp:branch_dp
              ~nbd_uri:branch_nbd_uri ~counter ~total child
        )
      )
      inactive_children
  in
  (* Then process the active continuation, reusing [working_vdi]
     unchanged so the storage-layer chain keeps advancing on the same
     leaf. There should be at most one active child; we recurse on all
     of them defensively in case of upstream data inconsistency. *)
  let active_relations =
    List.concat_map
      (fun child ->
        dfs_process_node ~ctx ~working_vdi ~working_dp ~nbd_uri ~counter ~total
          child
      )
      active_children
  in
  this_relation :: (inactive_relations @ active_relations)

module MIRROR : SMAPIv2_MIRROR = struct
  type context = unit

  let send_start _ctx ~dbg ~task_id:_ ~dp ~sr ~vdi ~image_format ~mirror_vm
      ~mirror_id ~local_vdi:_ ~copy_vm ~live_vm ~url ~remote_mirror ~dest_sr
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
    let snapshot_tree = get_snapshot_tree ~dbg ~vdi in
    let has_snapshots = snapshot_tree <> [] in
    if has_snapshots then
      SXM.info "%s found snapshot tree with %d node(s) to mirror" __FUNCTION__
        (count_tree_nodes snapshot_tree) ;

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
        let ctx =
          {
            dbg
          ; sr
          ; dest_sr
          ; url
          ; verify_dest
          ; mirror_vm
          ; copy_vm
          ; nbd_proxy_path
          ; image_format
          ; on_fail= ref []
          }
        in
        let run_on_fail () = List.iter D.log_and_ignore_exn !(ctx.on_fail) in
        try
          (* The dest VDI starts writable from receive_start3.
           If there are snapshots, switch to readonly for snapshot mirroring,
           DFS through the tree (cloning at revert branches and reusing
           [mirror_vdi] along the active path), then switch back to
           writable so the live leaf mirror can land into [mirror_vdi]
           directly. If no snapshots, the dest VDI is already writable — 
           proceed directly. *)
          let snapshot_relations =
            if has_snapshots then (
              switch_vdi ~mode:`Readonly ~dbg ~url ~verify_dest ~mirror_datapath
                ~dest_sr ~mirror_vdi ~mirror_vm ;
              let total = count_tree_nodes snapshot_tree in
              let counter = ref 0 in
              let relations =
                List.concat_map
                  (fun root ->
                    dfs_process_node ~ctx ~working_vdi:mirror_vdi
                      ~working_dp:mirror_datapath ~nbd_uri ~counter ~total root
                  )
                  snapshot_tree
              in
              SXM.info "%s %d snapshot(s) mirrored successfully" __FUNCTION__
                (List.length relations) ;
              switch_vdi ~mode:`Writable ~dbg ~url ~verify_dest ~mirror_datapath
                ~dest_sr ~mirror_vdi ~mirror_vm ;
              relations
            ) else
              []
          in
          D.debug "%s starting NBD proxy for leaf mirror" __FUNCTION__ ;
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
          wait_for_mirror ~dbg ~sr ~vdi ~vm:live_vm ~mirror_id
            ~error_msg:"Leaf VDI mirror failed during syncing" mk ;
          State.set_snapshot_mappings mirror_id snapshot_relations
        with
        | Storage_interface.Storage_error _ as e ->
            run_on_fail () ; raise e
        | e ->
            run_on_fail () ;
            D.error "%s caught exception during mirror: %s" __FUNCTION__
              (Printexc.to_string e) ;
            raise
              (Storage_interface.Storage_error
                 (Migration_mirror_failure (Printexc.to_string e))
              )
      )

  let receive_start _ctx ~dbg:_ ~sr:_ ~vdi_info:_ ~id:_ ~image_format:_
      ~similar:_ =
    Storage_interface.unimplemented __FUNCTION__

  let receive_start2 _ctx ~dbg:_ ~sr:_ ~vdi_info:_ ~id:_ ~image_format:_
      ~similar:_ ~vm:_ =
    Storage_interface.unimplemented __FUNCTION__

  let receive_start3 _ctx ~dbg ~sr ~vdi_info ~mirror_id ~image_format ~similar:_
      ~vm ~url ~verify_dest =
    D.debug
      "%s dbg: %s sr: %s vdi: %s id: %s image_format: %s vm: %s url: %s \
       verify_dest: %B"
      __FUNCTION__ dbg (s_of_sr sr)
      (string_of_vdi_info vdi_info)
      mirror_id image_format (s_of_vm vm) url verify_dest ;
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
