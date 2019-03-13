(*
 * Copyright (C) Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Lesser General Public License for more details.
 *)

open Db_filter_types
open Stdext
open Listext
open Monitor_types
open Monitor_dbcalls_cache
open Xstringext
open Threadext

module D = Debug.Make(struct let name = "monitor_dbcalls" end)
open D

let get_pif_and_bond_changes () =
  (* Read fresh PIF information from networkd. *)
  let open Network_stats in
  let stats = read_stats () in
  List.iter (fun (dev, stat) ->
      if not (String.startswith "vif" dev) then (
        if stat.nb_links > 1 then (* bond *)
          Hashtbl.add bonds_links_up_tmp dev stat.links_up;
        let pif = {
          pif_name = dev;
          pif_carrier = stat.carrier;
          pif_speed = stat.speed;
          pif_duplex = stat.duplex;
          pif_pci_bus_path = stat.pci_bus_path;
          pif_vendor_id = stat.vendor_id;
          pif_device_id = stat.device_id;
        } in
        Hashtbl.add pifs_tmp pif.pif_name pif;
      )
    ) stats;
    (* Check if any of the PIFs have changed since our last reading. *)
    let pif_changes = get_updates_values ~before:pifs_cached ~after:pifs_tmp in
    (* Check if any of the bonds have changed since our last reading. *)
    let bond_changes = get_updates_map ~before:bonds_links_up_cached ~after:bonds_links_up_tmp in
    (* Return lists of changes. *)
    (pif_changes, bond_changes)

let set_pif_changes ?except () =
  Mutex.execute pifs_cached_m (fun _ ->
      transfer_map ?except ~source:pifs_tmp ~target:pifs_cached
    )

let set_bond_changes ?except () =
  Mutex.execute bonds_links_up_cached_m (fun _ ->
      transfer_map ?except ~source:bonds_links_up_tmp ~target:bonds_links_up_cached
    )

(* This function updates the database for all the slowly changing properties
 * of host memory, VM memory, PIFs, and bonds.
*)
let pifs_update_fn () =
  let pif_changes, bond_changes = get_pif_and_bond_changes () in
  Server_helpers.exec_with_new_task "updating PIFs"
    (fun __context ->
       let host = Helpers.get_localhost ~__context in
       let issues = ref [] in

       let keeps = ref [] in
       List.iter (fun (bond, links_up) ->
           try
             let my_bond_pifs = Db.PIF.get_records_where ~__context
                 ~expr:(And (And (Eq (Field "host", Literal (Ref.string_of host)),
                                  Not (Eq (Field "bond_master_of", Literal "()"))),
                             Eq(Field "device", Literal bond))) in
             let my_bonds = List.map (fun (_, pif) -> List.hd pif.API.pIF_bond_master_of) my_bond_pifs in
             if (List.length my_bonds) <> 1 then
               debug "Error: bond %s cannot be found" bond
             else
               Db.Bond.set_links_up ~__context ~self:(List.hd my_bonds)
                 ~value:(Int64.of_int links_up)
           with e ->
             issues := e :: !issues;
             keeps := bond :: !keeps
         ) bond_changes;
       set_bond_changes ~except:!keeps ();

       begin
         try
           Monitor_master.update_pifs ~__context host pif_changes;
           set_pif_changes ();
         with e ->
           issues := e :: !issues
       end;

       List.iter (function
           | Db_exn.Read_missing_uuid _ -> ()
           | e -> error "pifs_and_memory_update issue: %s" (ExnHelper.string_of_exn e)
         ) (List.rev !issues)
    )

let monitor_dbcall_thread () =
  while true do
    try
      pifs_update_fn ();
      Monitor_mem_host.update ();
      Monitor_mem_vms.update ();
      Monitor_pvs_proxy.update ();
      Thread.delay 5.
    with e ->
      info "monitor_dbcall_thread would have died from: %s; restarting in 30s."
        (ExnHelper.string_of_exn e);
      Thread.delay 30.
  done
