(*
 * Copyright (C) 2006-2011 Citrix Systems Inc.
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
module D=Debug.Make(struct let name="xapi" end)
open D

open Stdext
open Listext
open Xstringext

type base_class =
  | Storage_controller
  | Network_controller
  | Display_controller

let is_class_of_kind kind id =
  let base_class_id_of_kind = function
    | Storage_controller -> 0x0100
    | Network_controller -> 0x0200
    | Display_controller -> 0x0300
  in
  (* The base_class is the most-significant byte of the class ID *)
  id land 0xff00 = base_class_id_of_kind kind

let managed_classes = [
  Storage_controller;
  Network_controller;
  Display_controller;
]

let string_of_pci ~__context ~self =
  let pci = Db.PCI.get_record_internal ~__context ~self in
  String.concat "/" [pci.Db_actions.pCI_vendor_id; pci.Db_actions.pCI_device_id]

(* We use ints within code but schema uses hex strings _without_ leading '0x' *)
let int_of_id string_id =
  let int_of_hex_str = fun s -> Scanf.sscanf s "%x" (fun x -> x) in
  int_of_hex_str string_id
let id_of_int hex_id =
  Printf.sprintf "%04x" hex_id

let create ~__context ~class_id ~class_name ~vendor_id ~vendor_name ~device_id
    ~device_name ~host ~pci_id ~functions ~physical_function
    ~dependencies ~other_config
    ~subsystem_vendor_id ~subsystem_vendor_name
    ~subsystem_device_id ~driver_name ~subsystem_device_name =
  let p = Ref.make () in
  let uuid = Uuid.to_string (Uuid.make_uuid ()) in
  Db.PCI.create ~__context ~ref:p ~uuid ~class_id ~class_name ~vendor_id ~vendor_name ~device_id
    ~device_name ~host ~pci_id ~functions ~physical_function
    ~dependencies:[] ~other_config:[]
    ~subsystem_vendor_id ~subsystem_vendor_name
    ~subsystem_device_id ~driver_name ~subsystem_device_name
    ~scheduled_to_be_attached_to:Ref.null;
  debug "PCI %s, %s, %s created" pci_id vendor_name device_name;
  p

let get_local ~__context getter =
  let localhost = Helpers.get_localhost ~__context in
  let expr = Db_filter_types.(Eq (Field "host", Literal (Ref.string_of localhost))) in
  getter ~__context ~expr

let get_local_pcis_and_records ~__context =
  get_local ~__context Db.PCI.get_internal_records_where

let get_local_pci_refs ~__context =
  get_local ~__context Db.PCI.get_refs_where

(** Update pf and vf settings *)
(* For virtual function record, set field `physical_function` to its PF PCI record *)
(* For physical function record, set field `functions` to 1 plus number of its virtual functions *)
let update_pf_vf_relations ~__context ~pcis =
  let pci_path x = Printf.sprintf "/sys/bus/pci/devices/%s/physfn" x
  in
  let is_phy_fn pci_rec =
    let path = pci_path pci_rec.Db_actions.pCI_pci_id in
    try
      (*if can't read link from the path,then it's a physical function*)
      let _ = Unix.readlink path in
      false
    with _ -> true
  in
  let set_phy_fn (vf_ref, vf_rec) pfs =
    let path = pci_path vf_rec.Db_actions.pCI_pci_id in
    let phyfn_id = Filename.basename (Unix.readlink path) in
    try
      let pf, _ = List.find (fun (_, pf_rec) -> phyfn_id = pf_rec.Db_actions.pCI_pci_id) pfs in
      Db.PCI.set_physical_function ~__context ~self:vf_ref ~value:pf
    with Not_found ->
      error "Failed to find physical function of vf %s" vf_rec.Db_actions.pCI_uuid;
      raise Api_errors.(Server_error (internal_error, [Printf.sprintf "Cannot find pf for vf %s" (Ref.string_of vf_ref)]))
  in
  let pfs, vfs = List.partition (fun (_, pci_rec) -> is_phy_fn pci_rec) pcis in
  (* set physical function for vfs *)
  List.iter (fun (vf_ref, vf_rec) -> if vf_rec.Db_actions.pCI_physical_function = Ref.null then set_phy_fn (vf_ref, vf_rec) pfs) vfs;
  (* update pf's functions *)
  List.iter (fun (pf_ref, pf_rec) ->
      let vf_cnt = List.length pf_rec.Db_actions.pCI_virtual_functions in
      Db.PCI.set_functions ~__context ~self:pf_ref ~value:(Int64.of_int (vf_cnt + 1))
    ) pfs

let update_pcis ~__context =
  let host = Helpers.get_localhost ~__context in
  let existing = List.filter_map
      (fun pref ->
         let prec = Db.PCI.get_record_internal ~__context ~self:pref in
         if prec.Db_actions.pCI_host = host then
           Some (pref, prec)
         else
           None)
      (Db.PCI.get_all ~__context)
  in

  let open Xapi_pci_helpers in
  let strings_of_pci_property = function
    | None -> "", ""
    | Some property -> id_of_int property.id, property.name
  in
  let string_of_pci_driver_name = function
    | None -> ""
    | Some name -> name
  in
  let rec update_or_create cur = function
    | [] -> cur
    | pci :: remaining_pcis ->
      let obj =
        try
          let (subsystem_vendor_id, subsystem_vendor_name) =
            strings_of_pci_property pci.subsystem_vendor in
          let (subsystem_device_id, subsystem_device_name) =
            strings_of_pci_property pci.subsystem_device in
          let driver_name = string_of_pci_driver_name pci.driver_name in
          let (rf, rc) = List.find (fun (rf, rc) ->
              rc.Db_actions.pCI_pci_id = pci.address &&
              rc.Db_actions.pCI_vendor_id = id_of_int pci.vendor.id &&
              rc.Db_actions.pCI_device_id = id_of_int pci.device.id &&
              rc.Db_actions.pCI_subsystem_vendor_id = subsystem_vendor_id &&
              rc.Db_actions.pCI_subsystem_device_id = subsystem_device_id)
              existing in
          (* sync the vendor name. *)
          if rc.Db_actions.pCI_vendor_name <> pci.vendor.name
          then Db.PCI.set_vendor_name ~__context ~self:rf ~value:pci.vendor.name;
          (* sync the device name. *)
          if rc.Db_actions.pCI_device_name <> pci.device.name
          then Db.PCI.set_device_name ~__context ~self:rf ~value:pci.device.name;
          (* sync the subsystem vendor name. *)
          if rc.Db_actions.pCI_subsystem_vendor_name <> subsystem_vendor_name
          then Db.PCI.set_subsystem_vendor_name ~__context ~self:rf ~value:subsystem_vendor_name;
          (* sync the subsystem device name. *)
          if rc.Db_actions.pCI_subsystem_device_name <> subsystem_device_name
          then Db.PCI.set_subsystem_device_name ~__context ~self:rf ~value:subsystem_device_name;
          (* sync the driver name. *)
          if rc.Db_actions.pCI_driver_name <> driver_name
          then Db.PCI.set_driver_name ~__context ~self:rf ~value:driver_name;
          (* sync the class information. *)
          if rc.Db_actions.pCI_class_id <> id_of_int pci.pci_class.id
          then Db.PCI.set_class_id ~__context ~self:rf ~value:(id_of_int pci.pci_class.id);
          if rc.Db_actions.pCI_class_name <> pci.pci_class.name
          then Db.PCI.set_class_name ~__context ~self:rf ~value:pci.pci_class.name;
          (* sync the attached VMs. *)
          let attached_VMs = List.filter (Db.is_valid_ref __context) rc.Db_actions.pCI_attached_VMs in
          if attached_VMs <> rc.Db_actions.pCI_attached_VMs then
            Db.PCI.set_attached_VMs ~__context ~self:rf ~value:attached_VMs;
          rf, rc
        with Not_found ->
          let subsystem_vendor_id, subsystem_vendor_name =
            strings_of_pci_property pci.subsystem_vendor in
          let subsystem_device_id, subsystem_device_name =
            strings_of_pci_property pci.subsystem_device in
          let driver_name = string_of_pci_driver_name pci.driver_name in
          let self = create ~__context
              ~class_id:(id_of_int pci.pci_class.id)
              ~class_name:pci.pci_class.name
              ~vendor_id:(id_of_int pci.vendor.id)
              ~vendor_name:pci.vendor.name
              ~device_id:(id_of_int pci.device.id)
              ~device_name:pci.device.name ~host ~pci_id:pci.address
              ~functions:1L ~physical_function:Ref.null ~dependencies:[] ~other_config:[]
              ~subsystem_vendor_id ~subsystem_vendor_name
              ~subsystem_device_id ~subsystem_device_name ~driver_name in
          self, Db.PCI.get_record_internal ~__context ~self
      in
      update_or_create ((obj, pci) :: cur) remaining_pcis
  in
  let host_pcis = Xapi_pci_helpers.get_host_pcis () in
  let class_pcis =
    List.filter (fun pci ->
        List.exists (fun k -> is_class_of_kind k pci.pci_class.id) managed_classes
      ) host_pcis in
  let deps = List.flatten (List.map (fun pci -> pci.related) class_pcis) in
  let deps = List.map (fun dep -> List.find (fun pci -> pci.address = dep) host_pcis) deps in
  let managed_pcis = List.setify (class_pcis @ deps) in
  let current = update_or_create [] managed_pcis in

  let update_dependencies current =
    let rec update = function
      | [] -> ()
      | ((pref, prec), pci) :: remaining ->
        let dependencies = List.map
            (fun address ->
               let (r, _), _ = List.find (fun ((_, rc), _) -> rc.Db_actions.pCI_pci_id = address) current
               in r)
            pci.related
        in
        Db.PCI.set_dependencies ~__context ~self:pref ~value:dependencies;
        update remaining
    in
    update current
  in
  update_dependencies current;

  let current = List.map (fun ((pref, prec), _) -> pref, prec) current in
  let obsolete = List.set_difference existing current in
  List.iter (fun (self, _) -> Db.PCI.destroy ~__context ~self) obsolete;
  update_pf_vf_relations ~__context ~pcis:current

let with_vga_arbiter ~readonly f =
  Unixext.with_file
    "/dev/vga_arbiter"
    (if readonly then [Unix.O_RDONLY] else [Unix.O_RDWR])
    0o000
    f

let disable_system_display_device () =
  with_vga_arbiter ~readonly:false
    (fun fd -> Unixext.really_write_string fd "decodes none")

let get_system_display_device () =
  try
    let line =
      with_vga_arbiter ~readonly:true (fun fd ->
          let data = Unixext.try_read_string ~limit:1024 fd in
          List.hd (String.split ~limit:2 '\n' data)
        )
    in
    (* Example contents of line:
       		 * count:7,PCI:0000:10:00.0,decodes=io+mem,owns=io+mem,locks=none(0:0) *)
    let items = String.split ',' line in
    List.fold_left
      (fun acc item ->
         if String.startswith "PCI" item
         then Some (Scanf.sscanf item "PCI:%s" (fun id -> id))
         else acc)
      None items
  with _ -> None
