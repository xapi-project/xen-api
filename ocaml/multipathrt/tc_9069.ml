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
open Netapp_utils

let num_plug_unplug_iterations = 10
let cleanup = true (* clean up after the test *)

let plug_unplug_vdi rpc session vdi =
  let vbds = Client.VDI.get_VBDs rpc session vdi in
  List.iter
    (fun vbd ->
      Client.VBD.unplug rpc session vbd;
      Client.VBD.plug rpc session vbd;
    ) vbds

let run_netapp rpc session =
  let master = get_master rpc session in
  if not(is_multipathing_enabled rpc session master) then enable_multipathing rpc session master;

  let initial_table = get_devmapper_table rpc session master in
  List.iter (fun entry -> debug "Initial devmapper table entry: %s" (entry_to_string entry)) initial_table;

  let netapp_sr = get_netapp_sr rpc session in
  debug "Found NetApp SR: %s" (Ref.string_of netapp_sr);

  (* Find out the number of paths to it from this host *)
  let num_paths = List.length (get_paths_to_netapp_sr rpc session netapp_sr master) in

  (* Add a VDI *)
  debug "Creating test VDI...";
  let vdi = create_test_vdi rpc session netapp_sr in

  finally
    (fun _ ->
      (* Plug the VDI into dom0 so that we can find out its SCSIid *)
      debug "Plugging VDI into dom0...";
      let vbd = plug_vdi_to_dom0 rpc session vdi in
    
      finally
        (fun _ ->
          (* Get the VDI's SCSIid from the SR's sm-config field. *)
          debug "Getting VDI's SCSIid...";
          let scsiid = get_netapp_vdi_scsiid rpc session netapp_sr vdi in
          debug "VDI has SCSIid %s" scsiid;
       
          check_devmapper_table rpc session master initial_table scsiid num_paths;
          for i = 1 to num_plug_unplug_iterations do
            debug "Exercise iteration %d of %d..." i num_plug_unplug_iterations;
            plug_unplug_vdi rpc session vdi;
            check_devmapper_table rpc session master initial_table scsiid num_paths;
          done
        )
        (fun _ ->
          debug "Unplugging VBD...";
          Client.VBD.unplug ~rpc ~session_id:session ~self:vbd
        )
    )
    (fun _ ->
      debug "Removing test VDI...";
      Client.VDI.destroy ~rpc ~session_id:session ~self:vdi
    );
  ()
