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
open Stdext.Xstringext

module D=Debug.Make(struct let name="xva" end)
open D

(** A more descriptive exception to throw rather than 'Not_found' *)
exception Missing_key of string
let assoc key pairs = try List.assoc key pairs with Not_found -> raise (Missing_key key)

(** Represents the export format used in Zurich: *)

let xml_filename = "ova.xml"
let checksum_filename = "checksum.xml"

exception Parse_failure of string
exception Version_mismatch

type variety = [ `system | `ephemeral | `user | `suspend | `crashdump | `ha_statefile | `metadata ]
let variety_of_string x = match (String.lowercase_ascii x) with
  | "system" -> `system | "ephemeral" -> `ephemeral | "user" -> `user | "suspend" -> `suspend | "crashdump" -> `crashdump | "metadata" -> `metadata
  | x -> raise (Parse_failure (Printf.sprintf "Unknown variety: %s" x))
let string_of_variety = function
  | `system -> "system" | `ephemeral -> "ephemeral" | `user -> "user" | `suspend -> "suspend" | `crashdump -> "crashdump" | `ha_statefile -> "ha_statefile" | `metadata -> "metadata"

type vdi = { vdi_name: string;
             size: int64;
             source: string;
             ty: string;
             variety: variety }

type funct = Root | Unknown
let funct_of_string x = match (String.lowercase_ascii x) with
  | "root" -> Root | _ -> Unknown
let string_of_funct = function
  | Root -> "root" | _ -> "unknown"

type mode = [ `RO | `RW ]
let mode_of_string x = match (String.lowercase_ascii x) with
  | "rw" | "w" -> `RW | "r" -> `RO | x -> raise (Parse_failure (Printf.sprintf "Unknown mode: %s" x))
let string_of_mode = function
  | `RW -> "rw" | `RO -> "r"

type vbd = { device: string;
             funct: funct;
             mode: mode;
             vdi: vdi }

type vm = { vm_name: string;
            description: string;
            memory: int64;
            vcpus: int;
            is_hvm: bool;
            kernel_boot_cmdline: string;
            distrib: string option;
            distrib_version: string option;
            vbds: vbd list }

let total_size_of_disks vdis = List.fold_left Int64.add 0L (List.map (fun vdi -> vdi.size) vdis)

(* convert a vms, vdis representation list into xml *)
let to_xml (vms, vdis) =
  let xml_of_vdi vdi =
    let attrs = [ "name", vdi.vdi_name;
                  "size", Int64.to_string vdi.size;
                  "source", vdi.source;
                  "type", vdi.ty;
                  "variety", string_of_variety vdi.variety ] in
    Xml.Element("vdi", attrs, [])
  in
  let vdis = List.map xml_of_vdi vdis in

  let xml_of_vbd vbd =
    let attrs = [ "device", vbd.device;
                  "function", string_of_funct vbd.funct;
                  "mode", string_of_mode vbd.mode;
                  "vdi", vbd.vdi.vdi_name ] in
    Xml.Element("vbd", attrs, [])
  in
  let xml_of_vm vm =
    let label = Xml.Element("label", [], [ Xml.PCData vm.vm_name ]) in
    let description = Xml.Element("shortdesc", [], [ Xml.PCData vm.description ]) in
    let config = Xml.Element("config", [ "mem_set", Int64.to_string vm.memory;
                                         "vcpus", string_of_int vm.vcpus ], []) in
    let hacks = Xml.Element("hacks", [ "is_hvm", string_of_bool vm.is_hvm;
                                       "kernel_boot_cmdline", vm.kernel_boot_cmdline ], []) in
    let vbds = List.map xml_of_vbd vm.vbds in
    Xml.Element("vm", [ "name", vm.vm_name ],
                [ label; description; config; hacks ] @ vbds)
  in
  let vms = List.map xml_of_vm vms in
  Xml.Element("appliance", [ "version", "0.1" ], vms @ vdis)

let parse_appliance attrs children =
  let version = assoc "version" attrs in
  if version <> "0.1" then
    raise Version_mismatch;

  let find_all name xs =
    let f x =
      match x with
      | Xml.Element(name', _, _) as e when name = name' ->
        [ e ]
      | _ ->
        []
    in
    List.concat (List.map f xs)
  in
  let vdis = List.map (fun node ->
      match node with
      | Xml.Element("vdi", attrs, _) ->
        let name = assoc "name" attrs
        and size = Int64.of_string (assoc "size" attrs)
        and source = assoc "source" attrs
        and ty = assoc "type" attrs
        and variety = variety_of_string (assoc "variety" attrs) in
        { vdi_name = name; size = size; source = source; ty = ty; variety = variety }
      | _ -> raise (Parse_failure "expected VDI"))
      (find_all "vdi" children) in

  (* make an assocation list of vdi names -> vdis *)
  let vdi_table = List.map (fun vdi -> vdi.vdi_name, vdi) vdis in

  (* then VMs *)
  let find_element name xs =
    match (find_all name xs) with
    | element :: _ -> element
    | [] -> raise (Parse_failure (Printf.sprintf "Failed to find element: %s" name)) in
  let child_string node =
    match node with
    | Xml.Element(_, _, [ Xml.PCData x ]) -> String.strip String.isspace x
    | Xml.Element(_, _, []) -> ""
    | _ -> raise (Parse_failure (Printf.sprintf "Failed to find PCData within element")) in

  let vmconfig_of_xml node =
    match node with
    | Xml.Element("vm", _, children) ->
      let name = child_string(find_element "label" children) in
      let description = child_string(find_element "shortdesc" children) in
      let memory, vcpus, distrib, distrib_version = match find_element "config" children with
        | Xml.Element(_, attrs, _) ->
          assoc "mem_set" attrs, assoc "vcpus" attrs,
          (try Some (assoc "distrib" attrs) with _ -> None),
          (try Some (assoc "distrib_version" attrs) with _ -> None)
        | _ -> raise (Parse_failure "Failed to find element: config") in
      let default_assoc default key pairs = try List.assoc key pairs with Not_found -> default in
      (* make HVM the default if nothing is specified *)
      let is_hvm, cmdline = match find_element "hacks" children with
        | Xml.Element(_, attrs, _) ->
          default_assoc "true" "is_hvm" attrs,
          default_assoc "" "kernel_boot_cmdline" attrs
        | _ -> "true", "" in

      let vbds = find_all "vbd" children in

      let vbdconfig_of_xml node =
        match node with
        | Xml.Element("vbd", attrs, _) ->
          let device = assoc "device" attrs
          and funct = funct_of_string (assoc "function" attrs)
          and mode = mode_of_string (assoc "mode" attrs)
          and vdi = assoc "vdi" attrs in
          let vdi = assoc vdi vdi_table in
          {
            device = device;
            funct = funct;
            mode = mode;
            vdi = vdi
          }
        | _ -> raise (Parse_failure "expected VBD")
      in

      let vbds = List.map vbdconfig_of_xml vbds in
      {
        vm_name = name;
        description = description;
        memory = Int64.of_string memory;
        vcpus = int_of_string vcpus;
        is_hvm = (String.lowercase_ascii is_hvm) = "true";
        kernel_boot_cmdline = cmdline;
        vbds = vbds;
        distrib = distrib;
        distrib_version = distrib_version
      }
    | _ -> raise (Parse_failure "expected VM")
  in

  let vms = List.map vmconfig_of_xml (find_all "vm" children) in
  vms, vdis

(* convert xml to a vm/vdi config representation *)
let of_xml node =
  match node with
  | Xml.Element("appliance", attrs, children) ->
    parse_appliance attrs children
  | _ -> raise (Parse_failure "expected appliance or vm")

(** Return true if <path> looks like a Zurich/Geneva style XVA *)
let is_valid path =
  let stats = Unix.LargeFile.stat path in
  if stats.Unix.LargeFile.st_kind <> Unix.S_DIR then false
  else begin
    let meta_path = Filename.concat path xml_filename in
    let stats = Unix.stat meta_path in
    if stats.Unix.st_kind <> Unix.S_REG then false
    else begin
      try
        let xml = Xml.parse_file meta_path in
        ignore(of_xml xml);
        true
      with _ -> false
    end
  end

(** Transmit a Zurich/Geneva style XVA at <path> to the server *)
let send path fd =
  let is_dir path = let stat = Unix.stat path in stat.Unix.st_kind = Unix.S_DIR in
  let add path (* actual path *) filename (* for tar header *) =
    debug "Attempting to add %s (%s)\n" path filename;
    let hdr = Tar_unix.Header.of_file path in
    let hdr = { hdr with Tar_unix.Header.file_name = filename } in
    debug "file_size = %Ld\n" (hdr.Tar_unix.Header.file_size);
    Tar_unix.write_block hdr
      (fun ofd ->
         let ifd = Unix.openfile path [Unix.O_RDONLY] 0o644 in
         Stdext.Pervasiveext.finally (fun () -> Tar_unix.Archive.copy_n ifd ofd hdr.Tar_unix.Header.file_size)
           (fun () -> Unix.close ifd)) fd in

  let add_disk path =
    let chunks = List.filter (fun x -> String.endswith ".gz" x) (Array.to_list (Sys.readdir path)) in
    let chunks = List.sort compare chunks in
    List.iter (fun chunk -> add (Filename.concat path chunk) (path ^ "/" ^ chunk)) chunks in

  Sys.chdir path;
  add xml_filename xml_filename;
  let disks = List.filter (fun x -> x <> xml_filename) (Array.to_list (Sys.readdir ".")) in
  (* Just in case: filter out non-directories and stuff prefixed with "." *)
  let disks = List.filter (fun x -> not(String.startswith "." x) && is_dir x) disks in
  List.iter add_disk disks;

  Tar_unix.write_end fd

