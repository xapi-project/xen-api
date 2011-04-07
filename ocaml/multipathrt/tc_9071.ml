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
(*
 * Set up multipathed device, e.g. LVHDoISCSISR with n paths
 * Attach VDI of this SR to dom0
 * Remove n-1 paths (iptables -A OUTPUT -d <IP> -j DROP)
 * Start a dd of a few megs of zeros to the VDI
 * Check how long the dd takes to finish
 *)

open Listext
open Pervasiveext
open Client
open Utils
open Iscsi_utils

let iscsi_luns = 3
let iscsi_vifs = 4
let sr_disk_size = Int64.mul 1048576L 1024L (* 1 GiB *)
let cleanup = true
let max_failover_time = 50. (* seconds *)

let run_iscsi rpc session =
  Random.self_init();

  let master = get_master rpc session in
  if not(is_multipathing_enabled rpc session master) then enable_multipathing rpc session master;

  let iscsi_vm = setup_iscsi_vm rpc session iscsi_luns iscsi_vifs sr_disk_size in
  finally
    (fun () ->
      wait_for_iscsi_vm_to_boot rpc session iscsi_vm;
      let (scsiid, sr) = setup_iscsi_sr rpc session master iscsi_vm in
      finally
        (fun () ->
          debug "Creating VDI in iSCSI SR...";
          let vdi = create_test_vdi rpc session sr in
          finally
            (fun () ->
              debug "Attaching VDI to dom0...";
              let vbd = plug_vdi_to_dom0 rpc session vdi in

              let time = time_data_transfer rpc session master vbd in
              debug " ** Duration of dd (with %d of %d paths) was %f seconds **" iscsi_vifs iscsi_vifs time;

              finally
                (fun () ->
                  let ip_addrs = get_vif_ip_addrs rpc session iscsi_vm in
                  debug "Found IP addresses [%s]" (String.concat "; " ip_addrs);
                  if List.length ip_addrs <> iscsi_vifs then raise (Multipathrt_exceptions.Test_error (Printf.sprintf "Expected to find %d VIFs on iSCSI target VM; found %d VIFs" iscsi_vifs (List.length ip_addrs)));
                  let paths_to_remove = remove_random_element ip_addrs in
                  modify_paths rpc session master paths_to_remove true;

                  finally
                    (fun () ->
                      let time = time_data_transfer rpc session master vbd in
                      debug " ** Duration of dd (with 1 of %d paths) was %f seconds **" iscsi_vifs time;
                      if time > max_failover_time then failwith "It took too long to failover between paths"
                    )
                    (fun () ->
                      debug "Reinstating removed paths...";
                      modify_paths rpc session master paths_to_remove false
                    )
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
