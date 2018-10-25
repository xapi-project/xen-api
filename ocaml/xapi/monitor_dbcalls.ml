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

let get_host_memory_changes xc =
  let physinfo = Xenctrl.physinfo xc in
  let bytes_of_pages pages =
    let kib = Xenctrl.pages_to_kib (Int64.of_nativeint pages) in
    Int64.shift_left kib 10
  in
  let free_bytes = bytes_of_pages physinfo.Xenctrl.free_pages in
  let total_bytes = bytes_of_pages physinfo.Xenctrl.total_pages in
  let host_memory_changed =
    !host_memory_free_cached <> free_bytes ||
    !host_memory_total_cached <> total_bytes
  in
  if host_memory_changed then Some (free_bytes, total_bytes) else None

let set_host_memory_change (free_bytes, total_bytes) =
  Mutex.execute host_memory_m (fun _ ->
      host_memory_free_cached := free_bytes;
      host_memory_total_cached := total_bytes;
    )

let get_vm_memory_changes xc =
  let domains = Xenctrl.domain_getinfolist xc 0 in
  let process_vm dom =
    let open Xenctrl in
    if not dom.dying then
      begin
        let uuid = Uuid.string_of_uuid (Uuid.uuid_of_int_array dom.handle) in
        let kib = Xenctrl.pages_to_kib (Int64.of_nativeint dom.total_memory_pages) in
        let memory = Int64.mul kib 1024L in
        Hashtbl.add vm_memory_tmp uuid memory
      end
  in
  List.iter process_vm domains;
  get_updates_map ~before:vm_memory_cached ~after:vm_memory_tmp

let set_vm_memory_changes ?except () =
  Mutex.execute vm_memory_cached_m (fun _ ->
      transfer_map ?except ~source:vm_memory_tmp ~target:vm_memory_cached
    )

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
let pifs_and_memory_update_fn xc =
  let host_memory_changes = get_host_memory_changes xc in
  let vm_memory_changes = get_vm_memory_changes xc in
  let pif_changes, bond_changes = get_pif_and_bond_changes () in
  Server_helpers.exec_with_new_task "updating VM_metrics.memory_actual fields and PIFs"
    (fun __context ->
       let host = Helpers.get_localhost ~__context in
       let issues = ref [] in

       let keeps = ref [] in
       List.iter (fun (uuid, memory) ->
           try
             let vm = Db.VM.get_by_uuid ~__context ~uuid in
             let vmm = Db.VM.get_metrics ~__context ~self:vm in
             if (Db.VM.get_resident_on ~__context ~self:vm = host)
             then Db.VM_metrics.set_memory_actual ~__context ~self:vmm ~value:memory
             else clear_cache_for_vm uuid
           with e ->
             issues := e :: !issues;
             keeps := uuid :: !keeps
         ) vm_memory_changes;
       set_vm_memory_changes ~except:!keeps ();

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

       begin
         match host_memory_changes with
         | None -> ()
         | Some (free, total as c) ->
           try
             let metrics = Db.Host.get_metrics ~__context ~self:host in
             Db.Host_metrics.set_memory_total ~__context ~self:metrics ~value:total;
             Db.Host_metrics.set_memory_free ~__context ~self:metrics ~value:free;
             set_host_memory_change c
           with e ->
             issues := e :: !issues
       end;

       List.iter (function
           | Db_exn.Read_missing_uuid _ -> ()
           | e -> error "pifs_and_memory_update issue: %s" (ExnHelper.string_of_exn e)
         ) (List.rev !issues)
    )

let monitor_dbcall_thread () =
  Xenctrlx.with_intf (fun xc ->
      while true do
        try
          pifs_and_memory_update_fn xc;
          Monitor_pvs_proxy.update ();
          Thread.delay 5.
        with e ->
          debug "monitor_dbcall_thread would have died from: %s; restarting in 30s."
            (ExnHelper.string_of_exn e);
          Thread.delay 30.
      done
    )
