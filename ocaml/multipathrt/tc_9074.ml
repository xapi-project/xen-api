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
(* Alert generation with path flapping
 *
 * Verify path flapping such that a path has been restored by the time the alert
 * is generated -> PBD.other-config records are accurate, alert message reflects
 * state of the system after restoration
 * 
 * # Sync with daemon (up to 2 mins)
 * 1. Take down path
 * 2. Wait for alert
 * 
 * # Ensure path is up at start of 'flap test' (2 mins)
 * 3. Restore path
 * 4. Wait for alert
 * 
 * # Flap test: (2 mins)
 * 5. Take down path
 * 6. Wait for 1 minute
 * 7. Restore path
 * 8. Wait for alert, check it's correct!
 *)

open Listext
open Pervasiveext
open Client
open Utils
open Iscsi_utils
open Alert_utils

let iscsi_luns = 3
let iscsi_vifs = 4
let sr_disk_size = Int64.mul 1048576L 1024L (* 1 GiB *)
let cleanup = true

let min_alert_timing_delay = 120. (* When syncronized with mpathalert, we should not receive two consecutive alerts within 2 minutes. *)
let max_alert_timing_delay = 180.(* Product requirements state "When a path fails, a system alert should be generated within 3 minutes". *)
let alert_timing_tolerance = 5. (* Tolerance, in seconds, for the wait for an alert. This is to give the benefit of the doubt for marginal cases, due to the round-trips necessary in the test. *)
let sleep_delay = 60.

let check_pbd_mpath_count rpc session pbd scsi_id current_paths max_paths =
  let pbd_other_config = Client.PBD.get_other_config rpc session pbd in
  (* Check that there's an 'mpath-<scsiid>' key there *)
  let key_name = Printf.sprintf "mpath-%s" scsi_id in
  if List.mem_assoc key_name pbd_other_config then begin
    debug "PBD.other-config key %s exists as expected" key_name;
    (* Check that it correctly says the number of active paths *)
    let expected_value = Printf.sprintf "[%d, %d]" current_paths max_paths in
    let actual_value = List.assoc key_name pbd_other_config in
    if expected_value = actual_value then
      debug "PBD.other-config mpath value is correct (%s)" actual_value
    else
      failwith (Printf.sprintf "PBD.other-config mpath value is wrong. Expected %s, got %s" expected_value actual_value)
  end else
    failwith (Printf.sprintf "Expected PBD.other-config key %s to exist" key_name)

let run rpc session =
  Random.self_init();

  let master = get_master rpc session in
  if not(is_multipathing_enabled rpc session master) then enable_multipathing rpc session master;

  let iscsi_vm = setup_iscsi_vm rpc session iscsi_luns iscsi_vifs sr_disk_size in
  finally
    (fun () ->
      wait_for_iscsi_vm_to_boot rpc session iscsi_vm;
      let (scsi_id, sr) = setup_iscsi_sr rpc session master iscsi_vm in
      finally
        (fun () ->
          debug "Creating VDI in iSCSI SR...";
          let vdi = create_test_vdi rpc session sr in
          finally
            (fun () ->
              debug "Attaching VDI to dom0...";
              let vbd = plug_vdi_to_dom0 rpc session vdi in

              finally
                (fun () ->
                  let ip_addrs = get_vif_ip_addrs rpc session iscsi_vm in
                  let num_paths = List.length ip_addrs in
                  debug "Found %d paths: [%s]" num_paths (String.concat "; " ip_addrs);
                  if num_paths <> iscsi_vifs then raise (Multipathrt_exceptions.Test_error (Printf.sprintf "Expected to find %d VIFs on iSCSI target VM; found %d VIFs" iscsi_vifs num_paths));

                  (* Remove a single path *)
                  let path_to_remove = select_random_element ip_addrs in
                  debug "Removing path %s..." path_to_remove;
                  modify_paths rpc session master [path_to_remove] true;
                  let modification_time = Unix.gettimeofday() in

                  (* Wait for an alert which says that the path we removed is unhealthy *)
                  wait_for_alert_saying_path_is_unhealthy rpc session scsi_id num_paths;
                  let alert_time = Unix.gettimeofday() in
                  check_delay_approx modification_time alert_time 0. max_alert_timing_delay alert_timing_tolerance;

                  (* Restore path *)
                  debug "Restoring path %s..." path_to_remove;
                  modify_paths rpc session master [path_to_remove] false;
                  let modification_time = Unix.gettimeofday() in

                  (* Wait for an alert saying the path has been restored *)
                  wait_for_alert_saying_all_is_well rpc session scsi_id num_paths;
                  let alert_time = Unix.gettimeofday() in
                  check_delay_approx modification_time alert_time min_alert_timing_delay max_alert_timing_delay alert_timing_tolerance;

                  (* Remove a single path *)
                  debug "Now for the flapping!";
                  debug "Removing path %s..." path_to_remove;
                  modify_paths rpc session master [path_to_remove] true;
                  let modification_time = Unix.gettimeofday() in

                  (* Wait for a minute *)
                  debug "Sleeping for a minute";
                  Unix.sleep (int_of_float sleep_delay);

                  (* Restore the path *)
                  debug "Restoring path %s..." path_to_remove;
                  modify_paths rpc session master [path_to_remove] false;

                  (* Wait for an alert *)
                  wait_for_alert_saying_we_flapped rpc session scsi_id num_paths;
                  let alert_time = Unix.gettimeofday() in
                  check_delay_approx modification_time alert_time (min_alert_timing_delay -. sleep_delay) max_alert_timing_delay alert_timing_tolerance;

                  (* When the alert is generated, check that the PBD.other-config says all paths are active *)
                  let pbds = Client.SR.get_PBDs rpc session sr in
                  match pbds with
                  | [] -> raise (Multipathrt_exceptions.Test_error "Could not find any PBDs for the lvmoiscsi SR")
                  | pbd::_ ->
                      check_pbd_mpath_count rpc session pbd scsi_id num_paths num_paths
                )
                (fun () ->
                  debug "Unplugging VBD...";
                  Client.VBD.unplug ~rpc ~session_id:session ~self:vbd
                )
            )
            (fun () ->
              debug "Removing test VDI...";
              Client.VDI.destroy ~rpc ~session_id:session ~self:vdi
            )
          )
        (fun () ->
          if cleanup then begin
            debug "Cleaning up SR...";
            destroy_sr rpc session sr
          end);
      debug "Finished")
    (fun () ->
      if cleanup then begin
        debug "Cleaning up VM...";
        destroy_vm rpc session iscsi_vm
      end)
