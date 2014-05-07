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
open Xstringext
open Log

(* Simple network tests *)

(* Tests we could still write:
   1. Create a new network, add two guests to it, ensure they can send frames to eachother
   2. Check you can't delete a network with interfaces still attached
*)

let log = Testlog.log

(* Our test networks all have this prefix on their names: *)
let name_prefix = "testnetwork"

(* Make a fresh name *)
let make_name = 
  let counter = ref 0 in
  fun () ->
    let name = name_prefix ^ (string_of_int !counter) in
    incr counter;
    name

(** Ensure we don't have existing networks with 'name_prefix' becaues this 
   might confuse the tests *)
let delete_existing_networks cli = 
  let existing_networks = Cliops.get_networks cli in
  List.iter (fun name ->
	       if String.startswith name_prefix name then begin
		   log Info "Deleting network: %s" name;
		   ignore(Cliops.remove_network cli name)
		 end
	    ) existing_networks

let run_n_times n f cli = 
  for i = 1 to n do
    f cli
  done

(** Create a network, check a bridge has been created. Remove the network
    and check the bridge has gone. Must be run from dom0. *)
let network_create_destroy cli vmid = 
  let list_bridge () = 
    let ifaces = List.filter Netdev.Link.is_up (Netdev.list ()) in
    List.filter (fun x -> Netdev.network.Netdev.exists x) ifaces in

  delete_existing_networks cli;
  
  (** Run (f x) and return (bridges created, bridges destroyed) *)
  let bridge_change f x = 
    let set_difference a b = List.filter (fun x -> not(List.mem x b)) a in
    let before = list_bridge () in
    let uuid = f x in
    let after = list_bridge () in
    (set_difference after before, (set_difference before after), uuid) in

  let add_network cli name =
    let newnet = Cliops.add_network cli name in
    let newvif = Cliops.add_nic cli vmid ("3","random",newnet) in
    ignore(Cliops.plug_nic cli newvif);
    (newnet,newvif) in
      
  let remove_network cli (netuuid,vifuuid) =
    let (_: string list) = Cliops.unplug_nic cli vifuuid in
    let (_: string list) = Cliops.remove_nic cli vifuuid in
    let (_: string list) = Cliops.remove_network cli netuuid in
    "" in
      
  (** Add a new network (with hopefully a fresh name) and return the name.
      Check that exactly one bridge is created. *)
  let add () = 
    let name = make_name () in
    let created, destroyed, uuids = bridge_change (add_network cli) name in
    (* print_endline (Printf.sprintf "created = %s" (List.hd created)); *)
    if destroyed <> [] || (List.length created <> 1)
    then failwith (Printf.sprintf "Expected exactly one bridge to be created during a network-add; instead we got: created = [ %s ]; destroyed = [ %s ]" (String.concat "; " created) (String.concat "; " destroyed));
    uuids in
  let remove uuids = 
    let created, destroyed,_ = bridge_change (remove_network cli) uuids in
    (* print_endline (Printf.sprintf "destroyed = %s" (List.hd destroyed)); *)
    if created <> [] || (List.length destroyed <> 1)
    then failwith (Printf.sprintf "Expected exactly one bridge to be destroyed during a network-remove; instead we got: created = [ %s ]; destroyed = [ %s ]" (String.concat "; " created) (String.concat "; " destroyed)) in

  let uuids = add () in
  remove uuids

let network_create_destroy n cli vmid = run_n_times n (network_create_destroy cli) vmid

let vlan_create_destroy cli = 
  delete_existing_networks cli;
  try
    (* make a fresh network reference *)
    let net = make_name () in
    log Info "Adding network %s" net;
    let net_uuid = Cliops.add_network cli net in
    
    (* Choose an existing PIF (with no VLAN tag!) to use as a base device *)
    let pifs = Cliops.get_pifs cli in
    if pifs = [] then failwith "Couldn't find an existing PIF to create a VLAN on";
    let pif : string = List.hd pifs in

    let pif_device = Cliops.get_pif_device cli pif in

    let tag : string = string_of_int (Random.int 100) in
    
    log Info "Adding PIF with vlan tag %s" tag;
    let pif_uuid = Cliops.create_vlan cli pif tag net_uuid in
    
    let ifaces = List.filter Netdev.Link.is_up (Netdev.list ()) in
    let expected_iface = pif_device ^ "." ^ tag in
    if not(List.mem expected_iface ifaces)
    then failwith (Printf.sprintf "Failed to find interface: %s" expected_iface);
    
    log Info "Deleting PIF with vlan tag %s" tag;
    let (_: string list) = Cliops.remove_pif cli pif_uuid in
    
    let ifaces = List.filter Netdev.Link.is_up (Netdev.list ()) in
    if List.mem expected_iface ifaces
    then failwith (Printf.sprintf "Failed to delete interface: %s" expected_iface);
    
    log Info "Deleting network %s" net;
    Cliops.remove_network cli net_uuid
  with e ->
    log Info "Caught exception: %s, cleaning up" (Printexc.to_string e);
    delete_existing_networks cli;
    raise e

let vlan_create_destroy n cli = run_n_times n vlan_create_destroy cli

(*
let _ = 
  let cli = Util.cli_onhost_with_pwd "xenroot" in
  for i = 0 to 100 do
    network_create_destroy cli;
    vlan_create_destroy cli
  done
*)

