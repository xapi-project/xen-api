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
open Client
open Xstringext

(* Resize failure modes advertised in LVHDSR.py *)
let resize_failure_modes = [
  "LVHDRT_inflate_after_create_journal";
  "LVHDRT_inflate_after_setSize";
  "LVHDRT_inflate_after_zeroOut";
  "LVHDRT_inflate_after_setSizePhys";
]

let clone_failure_modes = [
  "LVHDRT_clone_vdi_after_create_journal";
  "LVHDRT_clone_vdi_before_remove_journal";
  "LVHDRT_clone_vdi_before_undo_clone";
  "LVHDRT_clone_vdi_after_undo_clone";
]

(* Constants related to the helper plugin *)
let lvhdrt_plugin_pattern = "pattern"

(* The error code for the SR backend "failure" which signifies that the backend
 * exited via a FIST point. *)
let sr_fistpoint_failure = "SR_BACKEND_FAILURE_203"

(* Initial side of VDI to create *)
let initial_size = Globs.four_megs

(* Maximum increment for a VDI resize = 2 GB *)
let max_increment = Int64.mul 2048L Globs.meg

(* The largest size of VDI requested *)
let largest_size_demanded = ref 0L

(* Number of patterns supported by patterns.py. They are numbered 0 .. 3 *)
let num_patterns = 4

let choose_pattern () =
  Random.int num_patterns

let choose_variant () =
  Random.int64 (Int64.shift_left 1L 8) (* because it's just the bottom 8 bytes that are retained *)

let write_data rpc session host device size pattern variant =
  let result = Client.Host.call_plugin rpc session host Globs.helper_plugin lvhdrt_plugin_pattern [
    "dev", "/dev/" ^ device;
    "size", Int64.to_string size;
    "action", "write";
    "type", string_of_int pattern;
    "variant", Int64.to_string variant
  ] in
  if result <> "OK" then failwith "Could not write data"

let check_data rpc session host device size pattern variant =
  let result = Client.Host.call_plugin rpc session host Globs.helper_plugin lvhdrt_plugin_pattern [
    "dev", "/dev/" ^ device;
    "size", Int64.to_string size;
    "action", "read";
    "type", string_of_int pattern;
    "variant", Int64.to_string variant
  ] in
  if result <> "OK" then failwith "Data was not in the expected pattern"

let check_zeros rpc session host device offset length =
  let result = Client.Host.call_plugin rpc session host Globs.helper_plugin "check_zero" [
    "dev", "/dev/" ^ device;
    "offset", Int64.to_string offset;
    "length", Int64.to_string length;
  ] in
  if result <> "OK" then failwith "Did not find expected zeros in resized section"

let get_vdi_physical_size rpc session sr vdi =
  let master = Utils.get_master rpc session in
  let sruuid = Client.SR.get_uuid rpc session sr in
  let vdiuuid = Client.VDI.get_uuid rpc session vdi in
  let actual_size_str = Client.Host.call_plugin rpc session master Globs.helper_plugin "get_vdi_lsize" [ "sruuid", sruuid; "vdiuuid", vdiuuid ] in
  try
    Int64.of_string actual_size_str
  with Failure _ ->
    failwith "Error: could not determine size of VDI from 'lvs' via plugin"

let get_vdi_virtual_size rpc session vdi =
  Client.VDI.get_virtual_size ~rpc ~session_id:session ~self:vdi

let resize_vdi rpc session sr vdi current_size =
  (* Choose a random increment *)
  let increment = (if max_increment = 0L then 0L else Random.int64 max_increment) in
  current_size := Int64.add !current_size increment;
  largest_size_demanded := max !largest_size_demanded !current_size;
  Utils.debug " - Resizing VDI %s..." (Ref.string_of vdi);
  Utils.debug "    - adding %Ld so asking to resize to %Ld" increment !current_size;
  Pervasiveext.finally
    (fun () -> Client.VDI.resize ~rpc ~session_id:session ~vdi:vdi ~size:!current_size)
    (fun () -> 
      (* Now find out the actual size that was created, checking that it is at least what we asked for *)
      let new_size = get_vdi_virtual_size rpc session vdi in
      current_size := new_size;
      Utils.debug "    - new actual size is %Ld according to database" new_size
    )

let clone_vdi rpc session vdi =
  Utils.debug "Cloning VDI...";
  let new_vdi = Client.VDI.clone ~rpc ~session_id:session ~vdi:vdi ~driver_params:[] in
  new_vdi

let with_failure_mode rpc session host mode f =
  Utils.debug "Enabling FIST point %s..." mode;
  (* Use the LVHDRT_exit FIST point so that when the "mode" FIST point is
   * activated, the LVHD backend will die rather than merely sleep. *)
  Utils.with_fistpoint rpc session host "LVHDRT_exit"
    (fun () ->
      Utils.with_fistpoint rpc session host mode
        (fun () -> 
          try
            f ();
            Utils.debug " - Did not receive error"
          with
          | Api_errors.Server_error(sr_fistpoint_failure, _) -> Utils.debug " - Backend exited via FIST point %s." mode
          | e -> failwith (Printf.sprintf " - Received unexpected error: %s" (Printexc.to_string e))
        ) ()
    ) ();
  Utils.debug "Disabled FIST point %s" mode

(* Find a VBD device which is not currently in use *)
let get_unused_userdevice rpc session vm =
  let all_vbds = Client.VBD.get_all_records rpc session in
  let vm_vbds = List.filter (fun (vbd, vbd_rec) -> vbd_rec.API.vBD_VM = vm) all_vbds in
  let max_device = List.fold_left max 0 (List.map (fun (vbd, vbd_rec) -> int_of_string vbd_rec.API.vBD_userdevice) vm_vbds) in
  string_of_int (max_device+1)

(* Resize test.
 *
 * Checks that the journaller keeps enough information to ensure that when the
 * SR backend fails half-way through a resize, the data is still retained.
 *
 * Note that we grow the size of the disk for each failure point we exercise.
 * Each time we add a random increment, although in practise it will always
 * increase in multiples of 2 GB -- the value of RESIZE_INC in LVHDSR.py.
 *
 * Note that this test is not deterministic; the data written to the VDI is
 * a random selection from a choice of patterns.
 *
 * Description:
 *  - Create VDI
 *  - Attach to dom0
 *  - Write data into VDI of a certain pattern
 *  - Detach VBD
 *  - Resize VDI, inserting combinations of failures (so resize multiple times, each with a different type of failure mode, then finally resize with no failures)
 *  - Reattach VBD
 *  - Ensure pattern is the same at the start of the disk, and there are zeros in the expanded section
 *)
let resize_test rpc session =
  Random.self_init();

  let master = Utils.get_master rpc session in

  (* Find an LVHD SR *)
  let sr = Utils.find_lvhd_sr rpc session in

  let current_size = ref initial_size in

  (* Create VDI *)
  Utils.with_new_vdi rpc session sr !current_size "LVHD journalling test" "Test for resizing purposes"
    (fun vdi ->
      Utils.debug "VDI has uuid %s" (Client.VDI.get_uuid rpc session vdi);
      Utils.debug "VDI's initial virtual size is %Ld (according to database)" (get_vdi_virtual_size rpc session vdi);
      Utils.debug "VHD's physical size is %Ld (according to lvs)" (get_vdi_physical_size rpc session sr vdi);
      Utils.with_attached_vdi rpc session vdi
        (fun device vbd ->
          (* Write data into VDI of a certain pattern *)
          let pattern = choose_pattern () in
          let variant = choose_variant () in
          Utils.debug "Writing data with pattern %d into /dev/%s..." pattern device;
          write_data rpc session master device initial_size pattern variant;
        
          (* Detach VBD *)
          Utils.debug "Unplugging VBD...";
          Client.VBD.unplug ~rpc ~session_id:session ~self:vbd;
        
          (* Resize VDI, inserting combinations of failures *)
          List.iter
            (fun mode -> with_failure_mode rpc session master mode (fun () -> resize_vdi rpc session sr vdi current_size))
            resize_failure_modes;

          (* Now find out the physical size of the VDI, just in case the database is out of sync with reality and the VDI is larger than !current_size *)
          let new_size = get_vdi_physical_size rpc session sr vdi in
          Utils.debug "VHD's physical size is %Ld (according to lvs)" new_size;
          current_size := max new_size !largest_size_demanded;
          Utils.debug "Largest size requested so far was %Ld" !largest_size_demanded;
          Utils.debug "Next resize will be at least %Ld" !current_size;
      
          (* Now resize the VDI without inserting any failures *)
          Utils.debug "Now trying the final resize, which should succeed:";
          begin
            try
              resize_vdi rpc session sr vdi current_size
            with Api_errors.Server_error _ as x -> failwith (Printf.sprintf "Unexpectedly failed when resizing.\nError was: %s" (Printexc.to_string x))
          end;
        
          (* Re-attach VBD *)
          Utils.debug "Replugging VBD...";
          Client.VBD.plug ~rpc ~session_id:session ~self:vbd;
        
          (* Ensure pattern is the same at the start of the disk *)
          Utils.debug "Checking data has pattern %d in /dev/%s..." pattern device;
          check_data rpc session master device initial_size pattern variant;
          Utils.debug " - yes, it does.";

          (* And check that there are zeros in the expanded section *)
          let extra_size = Int64.sub !current_size initial_size in
          Utils.debug "Checking /dev/%s has %Ld zeros from offset %Ld" device extra_size initial_size;
          check_zeros rpc session master device initial_size extra_size;
          Utils.debug " - yes, it does.";
        )
    );
  Utils.debug "SUCCESS! Resize test passed!"

(* Clone/snapshot test.
 *
 * Checks that the journaller keeps enough information to ensure that when the
 * SR backend fails half-way through a clone, the data is still retained.
 *
 * Note that this test is not deterministic; the data written to the VDI is
 * a random selection from a choice of patterns.
 *
 * Description:
 *  - Create VDI
 *  - Attach VBD
 *  - Write data into VDI of a certain pattern
 *  - Detach VBD
 *  - Clone VDI, inserting combinations of failures
 *  - Reattach VBD, and also attach new VBD referencing the newly cloned VDI
 *  - Ensure pattern is the same on both clone and original
 *)
let clone_test rpc session =
  Random.self_init();

  let master = Utils.get_master rpc session in

  (* Find an LVHD SR *)
  let sr = Utils.find_lvhd_sr rpc session in

  let size = initial_size in

  (* Create VDI *)
  Utils.with_new_vdi rpc session sr size "LVHD journalling test" "Test for clone/snapshot purposes"
    (fun vdi ->
      Utils.with_attached_vdi rpc session vdi
        (fun device vbd ->
          (* Write data into VDI of a certain pattern *)
          let pattern = choose_pattern () in
          let variant = choose_variant () in
          Utils.debug "Writing data with pattern %d into /dev/%s..." pattern device;
          write_data rpc session master device size pattern variant;
        
          (* Detach VBD *)
          Client.VBD.unplug ~rpc ~session_id:session ~self:vbd;

          (* Clone VDI, inserting combinations of failures *)
          List.iter
            (fun mode -> with_failure_mode rpc session master mode (fun () -> clone_vdi rpc session vdi))
            clone_failure_modes;

          (* Now clone it without inserting any failures *)
          let cloned_vdi = 
            begin
              try
                clone_vdi rpc session vdi;
              with Api_errors.Server_error _ as x -> failwith (Printf.sprintf "Unexpectedly failed when cloning.\nError was: %s" (Printexc.to_string x))
            end in
        
          (* Re-attach the original VBD *)
          Client.VBD.plug ~rpc ~session_id:session ~self:vbd;
        
          (* Also attach new VBD referencing the newly cloned VDI *)
          Utils.with_attached_vdi rpc session cloned_vdi
            (fun device vbd ->
              (* Ensure pattern is the same on the clone *)
              Utils.debug "Checking data has pattern %d in /dev/%s..." pattern device;
              check_data rpc session master device size pattern variant;
              Utils.debug " - yes, it does.";
            );
        
          (* Ensure pattern is the same on the original *)
          Utils.debug "Checking data has pattern %d in /dev/%s..." pattern device;
          check_data rpc session master device size pattern variant;
          Utils.debug " - yes, it does.";
        )
    );
  Utils.debug "SUCCESS! Clone test passed!"
