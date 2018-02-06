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

module Rrdd = Rrd_client.Client

open Stdext
open Listext
open Monitor_types
open Db_filter_types
open Network

module D = Debug.Make(struct let name = "monitor_master" end)
open D

let update_configuration_from_master () =
  Server_helpers.exec_with_new_task "update_configuration_from_master" (fun __context ->
      let oc = Db.Pool.get_other_config ~__context ~self:(Helpers.get_pool ~__context) in
      let new_use_min_max = (List.mem_assoc Xapi_globs.create_min_max_in_new_VM_RRDs oc) &&
                            (List.assoc Xapi_globs.create_min_max_in_new_VM_RRDs oc = "true") in
      log_and_ignore_exn (fun () -> Rrdd.update_use_min_max new_use_min_max);
      let carrier = (List.mem_assoc Xapi_globs.pass_through_pif_carrier_key oc) &&
                    (List.assoc Xapi_globs.pass_through_pif_carrier_key oc = "true") in
      if !Xapi_globs.pass_through_pif_carrier <> carrier
      then debug "Updating pass_through_pif_carrier: New value=%b" carrier;
      Xapi_globs.pass_through_pif_carrier := carrier
    )

let get_pciids vendor device =
  (* FIXME : put a lazy cache *)
  let v, d = Pciutil.parse vendor device in
  (match v with None -> "" | Some x -> x),
  (match d with None -> "" | Some x -> x)

let set_pif_metrics ~__context ~self ~vendor ~device ~carrier ~speed ~duplex
    ~pcibuspath pmr =
  (* don't update & and reread pciids if db already contains same value *)
  if pmr.API.pIF_metrics_vendor_id <> vendor
  || pmr.API.pIF_metrics_device_id <> device then (
    let vendor_str, device_str = get_pciids vendor device in
    Db.PIF_metrics.set_vendor_id ~__context ~self ~value:vendor;
    Db.PIF_metrics.set_device_id ~__context ~self ~value:device;
    Db.PIF_metrics.set_vendor_name ~__context ~self ~value:vendor_str;
    Db.PIF_metrics.set_device_name ~__context ~self ~value:device_str;
  );
  if pmr.API.pIF_metrics_carrier <> carrier then
    Db.PIF_metrics.set_carrier ~__context ~self ~value:carrier;
  if pmr.API.pIF_metrics_speed <> speed then
    Db.PIF_metrics.set_speed ~__context ~self ~value:speed;
  if pmr.API.pIF_metrics_duplex <> duplex then
    Db.PIF_metrics.set_duplex ~__context ~self ~value:duplex;
  if pmr.API.pIF_metrics_pci_bus_path <> pcibuspath then
    Db.PIF_metrics.set_pci_bus_path ~__context ~self ~value:pcibuspath;
  Db.PIF_metrics.set_last_updated ~__context ~self
    ~value:(Date.of_float (Unix.gettimeofday ()))

(* Note that the following function is actually called on the slave most of the
 * time now but only when the PIF information changes. *)
let update_pifs ~__context host pifs =
  match List.length pifs with 0 -> () | _ ->
    (* Fetch all physical and bond PIFs from DB. *)
    let db_pifs = Db.PIF.get_records_where ~__context
        ~expr:(And (Eq (Field "host", Literal (Ref.string_of host)),
                    Or (Eq (Field "physical", Literal "true"),
                        Not (Eq (Field "bond_master_of", Literal "()"))))) in
    (* Iterate over them, and spot and update changes. *)
    List.iter (fun (pifdev, pifrec) ->
        begin try
            let pif_stats = List.find (fun p -> p.pif_name = pifrec.API.pIF_device) pifs in
            let carrier = pif_stats.pif_carrier in
            let speed = Int64.of_int pif_stats.pif_speed in
            let duplex = match pif_stats.pif_duplex with
              | Network_interface.Duplex_full -> true
              | Network_interface.Duplex_half -> false
              | Network_interface.Duplex_unknown -> false
            in
            let vendor = pif_stats.pif_vendor_id in
            let device = pif_stats.pif_device_id in
            let pcibuspath = pif_stats.pif_pci_bus_path in

            (* 1. Update corresponding VIF carrier flags *)
            if !Xapi_globs.pass_through_pif_carrier then begin
              try
                (* Go from physical interface -> bridge -> vif devices.
                   					 * Do this for the physical network and any VLANs/tunnels on top of it. *)
                let network = pifrec.API.pIF_network in
                let vlan_networks = List.map (fun vlan ->
                    let vlan_master = Db.VLAN.get_untagged_PIF ~__context ~self:vlan in
                    Db.PIF.get_network ~__context ~self:vlan_master
                  ) pifrec.API.pIF_VLAN_slave_of
                in
                let tunnel_networks = List.map (fun tunnel ->
                    let access_pif = Db.Tunnel.get_access_PIF ~__context ~self:tunnel in
                    Db.PIF.get_network ~__context ~self:access_pif
                  ) pifrec.API.pIF_tunnel_transport_PIF_of
                in
                let bridges = List.map (fun network -> Db.Network.get_bridge ~__context ~self:network)
                    (network :: vlan_networks @ tunnel_networks) in
                let dbg = Context.string_of_task __context in
                let ifs = List.flatten (List.map (fun bridge -> try Net.Bridge.get_interfaces dbg bridge with _ -> []) bridges) in
                let open Vif_device in
                let set_carrier vif =
                  if vif.pv then (
                    let open Xapi_xenops_queue in
                    let vm_uuid = fst vif.vif in
                    let queue_name = queue_of_vm ~__context ~self:(Db.VM.get_by_uuid ~__context ~uuid:vm_uuid) in
                    let module Client = (val make_client queue_name : XENOPS) in
                    Client.VIF.set_carrier dbg vif.vif carrier |> Xapi_xenops.sync __context queue_name
                  )
                in List.iter set_carrier (List.filter_map vif_device_of_string ifs)
              with e ->
                debug "Failed to update VIF carrier flags for PIF: %s" (ExnHelper.string_of_exn e)
            end;

            (* 2. Update database *)
            let metrics =
              (* If PIF metrics don't exist then create them *)
              if Db.is_valid_ref __context pifrec.API.pIF_metrics then
                pifrec.API.pIF_metrics
              else begin
                let ref = Ref.make() in
                Db.PIF_metrics.create ~__context ~ref ~uuid:(Uuid.to_string (Uuid.make_uuid ())) ~carrier:false
                  ~device_name:"" ~vendor_name:"" ~device_id:"" ~vendor_id:""
                  ~speed:0L ~duplex:false ~pci_bus_path:""
                  ~io_read_kbs:0. ~io_write_kbs:0. ~last_updated:(Date.of_float 0.)
                  ~other_config:[];
                Db.PIF.set_metrics ~__context ~self:pifdev ~value:ref;
                ref
              end
            in
            let pmr = Db.PIF_metrics.get_record ~__context ~self:metrics in
            set_pif_metrics ~__context ~self:metrics ~vendor ~device ~carrier ~speed:speed ~duplex:duplex
              ~pcibuspath pmr;
          with Not_found -> () end
      ) db_pifs
