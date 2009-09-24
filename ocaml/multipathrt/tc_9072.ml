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

open Pervasiveext
open Client
open Utils
open Netapp_utils

let max_failover_time = 50. (* seconds *)

let run_netapp rpc session =
  Random.self_init();

  let master = get_master rpc session in
  if not(is_multipathing_enabled rpc session master) then enable_multipathing rpc session master;

  let netapp_sr = get_netapp_sr rpc session in
  debug "Found NetApp SR: %s" (Ref.string_of netapp_sr);

  (* Find out the number of paths to it from this host *)
  let paths = get_paths_to_netapp_sr rpc session netapp_sr master in
  debug "Paths are [%s]" (String.concat "; " paths);
  let num_paths = List.length paths in

  debug "Creating VDI in NetApp SR...";
  let vdi = create_test_vdi rpc session netapp_sr in
  finally
    (fun () ->
      debug "Attaching VDI to dom0...";
      let vbd = plug_vdi_to_dom0 rpc session vdi in

      let time = time_data_transfer rpc session master vbd in
      debug " ** Duration of dd (with %d of %d paths) was %f seconds **" num_paths num_paths time;

      finally
        (fun () ->
          let paths_to_remove = remove_random_element paths in
          modify_paths rpc session master paths_to_remove true;

          finally
            (fun () ->
              let time = time_data_transfer rpc session master vbd in
              debug " ** Duration of dd (with 1 of %d paths) was %f seconds **" num_paths time;
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
