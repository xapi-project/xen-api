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
open Pervasiveext
open Client
open Utils
open Devmapper_utils
open Iscsi_utils

let iscsi_luns = 3
let iscsi_vifs = 4
let sr_disk_size = Int64.mul 1048576L 1024L (* 1 GiB *)
let num_plug_unplug_iterations = 10
let cleanup = true (* clean up after the test *)

let plug_unplug_sr rpc session sr =
  let pbds = Client.SR.get_PBDs rpc session sr in
  List.iter
    (fun pbd ->
      Client.PBD.unplug rpc session pbd;
      Client.PBD.plug rpc session pbd;
    ) pbds

let run_iscsi rpc session =
  let master = get_master rpc session in
  if not(is_multipathing_enabled rpc session master) then enable_multipathing rpc session master;

  let initial_table = get_devmapper_table rpc session master in
  List.iter (fun entry -> debug "Initial devmapper table entry: %s" (entry_to_string entry)) initial_table;

  let iscsi_vm = setup_iscsi_vm rpc session iscsi_luns iscsi_vifs sr_disk_size in
  finally
    (fun () ->
      wait_for_iscsi_vm_to_boot rpc session iscsi_vm;
      let (scsiid, sr) = setup_iscsi_sr rpc session master iscsi_vm in
      finally
        (fun () ->
          check_devmapper_table rpc session master initial_table scsiid iscsi_vifs;
          for i = 1 to num_plug_unplug_iterations do
            debug "Exercise iteration %d of %d..." i num_plug_unplug_iterations;
            plug_unplug_sr rpc session sr;
            check_devmapper_table rpc session master initial_table scsiid iscsi_vifs;
          done)
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
