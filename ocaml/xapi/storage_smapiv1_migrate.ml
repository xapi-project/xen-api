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
open Storage_interface
open Storage_migrate_helper
module State = Storage_migrate_helper.State
module SXM = Storage_migrate_helper.SXM

module type SMAPIv2_MIRROR = Storage_interface.MIRROR

module MIRROR : SMAPIv2_MIRROR = struct
  type context = unit

  let u x = raise Storage_interface.(Storage_error (Errors.Unimplemented x))

  let send_start _ctx = u __FUNCTION__

  let receive_start_common ~dbg ~sr ~vdi_info ~id ~similar ~vm =
    let on_fail : (unit -> unit) list ref = ref [] in
    let vdis = Local.SR.scan dbg sr in
    (* We drop cbt_metadata VDIs that do not have any actual data *)
    let vdis = List.filter (fun vdi -> vdi.ty <> "cbt_metadata") vdis in
    let leaf_dp = Local.DP.create dbg Uuidx.(to_string (make ())) in
    try
      let vdi_info = {vdi_info with sm_config= [("base_mirror", id)]} in
      let leaf = Local.VDI.create dbg sr vdi_info in
      D.info "Created leaf VDI for mirror receive: %s" (string_of_vdi_info leaf) ;
      on_fail := (fun () -> Local.VDI.destroy dbg sr leaf.vdi) :: !on_fail ;
      (* dummy VDI is created so that the leaf VDI becomes a differencing disk,
         useful for calling VDI.compose later on *)
      let dummy = Local.VDI.snapshot dbg sr leaf in
      on_fail := (fun () -> Local.VDI.destroy dbg sr dummy.vdi) :: !on_fail ;
      D.debug "%s Created dummy snapshot for mirror receive: %s" __FUNCTION__
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
            let vdi_clone = Local.VDI.clone dbg sr vdi in
            D.debug "Clone: %s" (Storage_interface.Vdi.string_of vdi_clone.vdi) ;
            ( if vdi_clone.virtual_size <> vdi_info.virtual_size then
                let new_size =
                  Local.VDI.resize dbg sr vdi_clone.vdi vdi_info.virtual_size
                in
                D.debug "Resize local clone VDI to %Ld: result %Ld"
                  vdi_info.virtual_size new_size
            ) ;
            vdi_clone
        | None ->
            D.debug "Creating a blank remote VDI" ;
            Local.VDI.create dbg sr vdi_info
      in
      D.debug "Parent disk content_id=%s" parent.content_id ;
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
            D.debug "Caught exception in on_fail: %s" (Printexc.to_string e)
        )
        !on_fail ;
      raise e

  let receive_start _ctx ~dbg ~sr ~vdi_info ~id ~similar =
    receive_start_common ~dbg ~sr ~vdi_info ~id ~similar ~vm:(Vm.of_string "0")

  let receive_start2 _ctx ~dbg ~sr ~vdi_info ~id ~similar ~vm =
    receive_start_common ~dbg ~sr ~vdi_info ~id ~similar ~vm

  let receive_finalize _ctx ~dbg ~id =
    let recv_state = State.find_active_receive_mirror id in
    let open State.Receive_state in
    Option.iter (fun r -> Local.DP.destroy dbg r.leaf_dp false) recv_state ;
    State.remove_receive_mirror id

  let receive_finalize2 _ctx ~dbg ~id =
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
        D.log_and_ignore_exn (fun () -> Local.VDI.destroy dbg r.sr r.dummy_vdi) ;
        Local.VDI.remove_from_sm_config dbg r.sr r.leaf_vdi "base_mirror"
      )
      recv_state ;
    State.remove_receive_mirror id

  let receive_cancel _ctx ~dbg ~id =
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
end
