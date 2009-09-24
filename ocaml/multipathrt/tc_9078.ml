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
(* Device-mapper table integrity for FibreChannel multipath *)

open Pervasiveext
open Client
open Utils
open Devmapper_utils
open Hba_utils

let num_plug_unplug_iterations = 10

let plug_unplug_sr rpc session sr =
  let pbds = Client.SR.get_PBDs rpc session sr in
  List.iter
    (fun pbd ->
      Client.PBD.unplug rpc session pbd;
      Client.PBD.plug rpc session pbd;
    ) pbds

let run_fc rpc session =
  let master = get_master rpc session in
  if not(is_multipathing_enabled rpc session master) then enable_multipathing rpc session master;

  let sr = get_fc_sr rpc session in
  debug "Found FibreChannel SR: %s" (Ref.string_of sr);
  let scsiid = get_fc_scsiid rpc session sr in
  debug "SCSIid is %s" scsiid;

  let num_paths = get_num_paths rpc session sr scsiid in
  debug "PBD.other-config indicates that there is a maximum of %d paths to this SR" num_paths;

  debug "Unplugging SR...";
  let pbds = Client.SR.get_PBDs rpc session sr in
  List.iter (fun pbd -> Client.PBD.unplug rpc session pbd) pbds;

  let initial_table = get_devmapper_table rpc session master in
  List.iter (fun entry -> debug "Initial devmapper table entry: %s" (entry_to_string entry)) initial_table;

  debug "Re-plugging SR...";
  List.iter (fun pbd -> Client.PBD.plug rpc session pbd) pbds;

  check_devmapper_table rpc session master initial_table scsiid num_paths;
  for i = 1 to num_plug_unplug_iterations do
    debug "Exercise iteration %d of %d..." i num_plug_unplug_iterations;
    plug_unplug_sr rpc session sr;
    check_devmapper_table rpc session master initial_table scsiid num_paths;
  done
