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

(* Restrictions *)
open Threadext
open License
module D = Debug.Debugger(struct let name="license" end)
open D

(* The restrictions that different licenses imply *)


(* The skus *)
type kind = Express | Server | Enterprise
type sku = Vanilla of kind | Dell of kind | HP of kind    

let kind_of_sku = function
  | Vanilla x -> x | Dell x -> x | HP x -> x

let is_floodgate_free x = 
  kind_of_sku x = Express (* Server and Enterprise are both 'floodgate paidfor' *)

let sku_of_string string = 
  match string with
      "XE Express"         -> Vanilla Express
    | "XE Server"          -> Vanilla Server
    | "XE Enterprise"      -> Vanilla Enterprise
    | "Dell XE Express"    -> Dell Express
    | "Dell XE Server"     -> Dell Server
    | "Dell XE Enterprise" -> Dell Enterprise
    | "HP XE Express"      -> HP Express
    | "HP XE Server"       -> HP Server
    | "HP XE Enterprise"   -> HP Enterprise
    | _ -> failwith (Printf.sprintf "Unknown SKU type: '%s'" string)

let string_of_sku =
  let string_of_kind = function
    | Express -> "Express" | Server -> "Server" | Enterprise -> "Enterprise" in
  function
  | Vanilla x -> "XE " ^ (string_of_kind x)
  | Dell x    -> "Dell XE " ^ (string_of_kind x)
  | HP x      -> "HP XE " ^ (string_of_kind x)

(* CA-26992: to avoid confusing the user with legacy SKU names we trivially obfuscate them *)
let obfuscated_string_of_sku =
  let string_of_kind = function
    | Express -> "F" (*free*) | (Server | Enterprise) -> "P" (*paid*) in
  function
  | Vanilla x -> "R" ^ (string_of_kind x)
  | Dell x    -> "D" ^ (string_of_kind x)
  | HP x      -> "H" ^ (string_of_kind x)

(* The restrictions that are applied *)
type restrictions = 
    {
      enable_vlans : bool;
      enable_qos : bool;
      enable_shared_storage : bool;
      enable_netapp : bool;
      enable_equalogic : bool;
      enable_pooling : bool;
      enable_xha: bool;
      enable_mtc_pci: bool;
      enable_email : bool;
      enable_performance : bool;
      enable_wlb : bool;
      enable_rbac : bool;
      restrict_connection : bool;
      platform_filter : bool;
      regular_nag_dialog : bool;
    }

(* Used for printing compact host x restriction tables *)
let to_compact_string (x: restrictions) = 
  (if x.enable_vlans            then "VLAN " else "     ") ^
    (if x.enable_qos            then "QoS  " else "     ") ^
    (if x.enable_shared_storage then "SStorage " else "         ") ^
    (if x.enable_netapp         then "NTAP " else "     ") ^
    (if x.enable_equalogic      then "EQL  " else "     ") ^
    (if x.enable_pooling        then "Pool " else "     ") ^
    (if x.enable_xha            then "XHA  " else "     ") ^
    (if x.enable_mtc_pci        then "MTC  " else "     ") ^
    (if x.enable_email          then "email " else "      ") ^
    (if x.enable_performance    then "perf " else "     ") ^
    (if x.enable_wlb            then "WLB  " else "     ") ^
    (if x.enable_rbac           then "RBAC " else "     ") ^
    (if x.restrict_connection   then "     " else "Cnx  ") ^
    (if x.platform_filter       then "     " else "Plat ") ^
    (if x.regular_nag_dialog    then " nag " else "     ")

(** Represents no restrictions at all *)
let most_permissive = 
  {
    enable_vlans = true;
    enable_qos = true;
    enable_shared_storage = true;
    enable_netapp = true;
    enable_equalogic = true;
    enable_pooling = true;
    enable_xha = true;
    enable_mtc_pci = true;
    enable_email = true;
    enable_performance = true;
    enable_wlb = true;
    enable_rbac = true;
    restrict_connection = false;
    platform_filter = false;
    regular_nag_dialog = false;
  }

(** Return a new restrictions record which, for each field, takes the least permissive of the two arguments *)
let least_permissive (a: restrictions) (b: restrictions) =
{
  enable_vlans          = a.enable_vlans          && b.enable_vlans;
  enable_qos            = a.enable_qos            && b.enable_qos;
  enable_shared_storage = a.enable_shared_storage && b.enable_shared_storage;
  enable_netapp         = a.enable_netapp         && b.enable_netapp;
  enable_equalogic      = a.enable_equalogic      && b.enable_equalogic;
  enable_pooling        = a.enable_pooling        && b.enable_pooling;
  enable_xha            = a.enable_xha            && b.enable_xha;
  enable_mtc_pci        = a.enable_mtc_pci        && b.enable_mtc_pci;
  enable_email          = a.enable_email          && b.enable_email;
  enable_performance    = a.enable_performance    && b.enable_performance;
  enable_wlb            = a.enable_wlb            && b.enable_wlb;
  enable_rbac           = a.enable_rbac           && b.enable_rbac;
  restrict_connection   = a.restrict_connection   || b.restrict_connection;
  platform_filter       = a.platform_filter       || b.platform_filter;
  regular_nag_dialog    = a.regular_nag_dialog    || b.regular_nag_dialog;
}

(** Return the 'pool_restrictions' being the greatest set of permissions allowed by all licenses *)
let pool_restrictions_of_list (hosts: restrictions list) = List.fold_left least_permissive most_permissive hosts

(** Returns true if the pool SKUs are 'floodgate free' (ie if any are express *)
let pool_is_floodgate_free_of_list (license_params: ((string * string) list) list) = 
  (* Some of the license_params might be malformed due to initial startup glitches *)
  let valid = List.filter (fun license_params -> try ignore(License.of_assoc_list license_params); true with _ -> false) license_params in
  let licenses = List.map License.of_assoc_list license_params in
  List.fold_left (||) false (List.map (fun x -> is_floodgate_free (sku_of_string x.License.sku)) licenses)

let _restrict_connection = "restrict_connection"
let _restrict_pooling = "restrict_pooling"
let _restrict_qos = "restrict_qos"
let _restrict_pool_attached_storage = "restrict_pool_attached_storage"
let _restrict_vlan = "restrict_vlan"
let _enable_xha = "enable_xha"
let _restrict_netapp = "restrict_netapp"
let _restrict_equalogic = "restrict_equalogic"
let _restrict_marathon = "restrict_marathon"
let _platform_filter = "platform_filter"
let _restrict_email_alerting = "restrict_email_alerting"
let _restrict_historical_performance = "restrict_historical_performance"
let _restrict_wlb = "restrict_wlb"
let _restrict_rbac = "restrict_rbac"
let _regular_nag_dialog = "regular_nag_dialog"

let to_assoc_list (x: restrictions) = 
  [ (_restrict_connection,string_of_bool x.restrict_connection);
    (_restrict_pooling,string_of_bool (not x.enable_pooling));
    (_restrict_qos,string_of_bool (not x.enable_qos));
    (_restrict_pool_attached_storage,string_of_bool (not x.enable_shared_storage));
    (_restrict_netapp, string_of_bool (not x.enable_netapp));
    (_restrict_equalogic, string_of_bool (not x.enable_equalogic));
    (_restrict_vlan,string_of_bool (not x.enable_vlans));
    (_enable_xha, string_of_bool (x.enable_xha));
    (_restrict_marathon, string_of_bool (not x.enable_mtc_pci));
    (_platform_filter, string_of_bool x.platform_filter);
    (_restrict_email_alerting, string_of_bool (not x.enable_email));
    (_restrict_historical_performance, string_of_bool (not x.enable_performance));
    (_restrict_wlb, string_of_bool (not x.enable_wlb));
    (_restrict_rbac, string_of_bool (not x.enable_rbac));
    (_regular_nag_dialog, string_of_bool x.regular_nag_dialog);
  ]

(* Read an association list possibly generated by a slave running a previous version and hence possibly 
   missing some values. In the case where a value is missing we default to the most_permissive. *)
let of_assoc_list x = 
  let find fn key = if List.mem_assoc key x then Some (fn (List.assoc key x)) else None in
  { 
    enable_vlans          = Opt.default most_permissive.enable_vlans          (Opt.map not (find bool_of_string _restrict_vlan));
    enable_qos            = Opt.default most_permissive.enable_qos            (Opt.map not (find bool_of_string _restrict_qos));
    enable_shared_storage = Opt.default most_permissive.enable_shared_storage (Opt.map not (find bool_of_string _restrict_pool_attached_storage));
    enable_netapp         = Opt.default most_permissive.enable_netapp         (Opt.map not (find bool_of_string _restrict_netapp));
    enable_equalogic      = Opt.default most_permissive.enable_equalogic      (Opt.map not (find bool_of_string _restrict_equalogic));
    enable_pooling        = Opt.default most_permissive.enable_pooling        (Opt.map not (find bool_of_string _restrict_pooling));
    enable_xha            = Opt.default most_permissive.enable_xha                         (find bool_of_string _enable_xha);
    enable_mtc_pci        = Opt.default most_permissive.enable_mtc_pci        (Opt.map not (find bool_of_string _restrict_marathon));
    restrict_connection   = Opt.default most_permissive.restrict_connection                (find bool_of_string _restrict_connection);
    platform_filter       = Opt.default most_permissive.platform_filter                    (find bool_of_string _platform_filter);
    enable_email          = Opt.default most_permissive.enable_email          (Opt.map not (find bool_of_string _restrict_email_alerting));
    enable_performance    = Opt.default most_permissive.enable_performance    (Opt.map not (find bool_of_string _restrict_historical_performance));
    enable_wlb            = Opt.default most_permissive.enable_wlb            (Opt.map not (find bool_of_string _restrict_wlb));
    enable_rbac           = Opt.default most_permissive.enable_rbac           (Opt.map not (find bool_of_string _restrict_rbac));
    regular_nag_dialog    = Opt.default most_permissive.regular_nag_dialog                 (find bool_of_string _regular_nag_dialog);
  }


(* Encodes the minimum set of restrictions available in all SKUs (ie FG-Free and FG-PaidFor) *)
let common_to_all_skus =
  {
    enable_vlans = true;
    enable_qos = true;
    enable_shared_storage = true;
    enable_netapp = false;
    enable_equalogic = false;
    enable_pooling = true;
    enable_xha = false;
    enable_mtc_pci = true;
    restrict_connection = false;
    platform_filter = true;
    enable_email = false;
    enable_performance = false;
    enable_wlb = false;
    enable_rbac = false;
    regular_nag_dialog = true;
  }

let get_sku () = sku_of_string !License.license.License.sku

let get_sku_from_license l = sku_of_string l.sku

let rec restrictions_of_sku = function
  | Vanilla Express -> common_to_all_skus
  | Vanilla (Server | Enterprise) ->
      {
	common_to_all_skus with
	  enable_xha = true;
	  platform_filter = false;
	  enable_netapp = true;
	  enable_equalogic = true;
	  enable_email = true;
	  enable_performance = true;
	  enable_wlb = true;
	  enable_rbac = true;
	  regular_nag_dialog = false;
      }
  | HP Express | Dell Express -> common_to_all_skus
  | HP x | Dell x ->
      let fallback = Vanilla x in
      warn "Falling back to SKU: '%s'" (string_of_sku (Vanilla x));
      restrictions_of_sku fallback 

let get () =
  restrictions_of_sku (get_sku ())

(** Cache of pool restrictions, always updated at least once when the master reads its license *)
let pool_restrictions = ref most_permissive
let pool_restrictions_m = Mutex.create ()

(** Called on the master to gate some operations *)
let get_pool () = Mutex.execute pool_restrictions_m (fun () -> !pool_restrictions)

(** Called whenever a slave resets its Host.license_params after reading in a license *)
let update_pool_restrictions ~__context = 
  Mutex.execute pool_restrictions_m
    (fun () ->
       let hosts = List.map (fun (_, host_r) -> host_r.API.host_license_params) (Db.Host.get_all_records ~__context) in
       let new_restrictions = pool_restrictions_of_list (List.map of_assoc_list hosts) in
       if new_restrictions <> !pool_restrictions then begin
	 info "Old pool restrictions: %s" (to_compact_string !pool_restrictions);
	 info "New pool restrictions: %s" (to_compact_string new_restrictions);
	 pool_restrictions := new_restrictions
       end
    )

let license_ok_for_pooling ~__context = 
  let rstr = get () in

  let slaves_exist = List.length (Db.Host.get_all ~__context) <> 1 in
  (* 'bad_master_license' if we're a master without a pooling license but there are slaves *)
  let bad_master_license = Pool_role.is_master () && not rstr.enable_pooling && slaves_exist in
  let bad_slave_license = not(Pool_role.is_master ()) && not rstr.enable_pooling in
  if bad_master_license
  then warn "This license does not support pooling and yet slaves exist in the database";
  if bad_slave_license
  then warn "This license does not support pooling and yet we are configured as a slave in a pool";
  
  not bad_master_license && (not bad_slave_license)

(* Whether WLB calls are enabled.  We use enable_pooling for this, just to
   save creating another flag.   The current intention is for WLB to be
   enabled only on Enterprise and Platinum. *)
let license_ok_for_wlb ~__context =
  (get_pool()).enable_wlb

let license_ok_for_rbac ~__context =
  (get_pool()).enable_rbac
  
