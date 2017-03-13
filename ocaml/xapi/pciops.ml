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
module D=Debug.Make(struct let name="pciops" end)
open D

open Stdext
open Listext
open Xstringext
open Threadext

let m = Mutex.create ()

(* http://wiki.xen.org/wiki/Bus:Device.Function_%28BDF%29_Notation *)
(* It might be possible to refactor this but attempts so far have failed. *)
let bdf_fmt            = format_of_string    "%04x:%02x:%02x.%01x"
let bdf_fmt_ignore     = format_of_string    "%_4x:%_2x:%_2x.%_1x%!"  (* with end-of-input match *)
let slash_bdf_scan_fmt = format_of_string "%d/%04x:%02x:%02x.%01x"
let slash_bdf_prnt_fmt = format_of_string "%d/%04x:%02x:%02x.%01x"
let bdf_paren_prnt_fmt = format_of_string   "(%04x:%02x:%02x.%01x)"
let bdf_paren_scan_fmt = format_of_string   "(%04x:%02x:%02x.%01x)"

let pcidev_of_pci ~__context pci =
  let bdf_str = Db.PCI.get_pci_id ~__context ~self:pci in
  Scanf.sscanf bdf_str bdf_fmt (fun a b c d -> (a, b, c, d))

let is_bdf_format str =
  try Scanf.sscanf str bdf_fmt_ignore true
  with Scanf.Scan_failure _ | Failure _ | End_of_file -> false

(* Confusion: the n/xxxx:xx:xx.x syntax originally meant PCI device
   xxxx:xx:xx.x should be plugged into bus number n. HVM guests don't have
   multiple PCI buses anyway. We reinterpret the 'n' to be a hotplug ordering *)
let sort_pcidevs devs =
  let ids = List.sort compare (Listext.List.setify (List.map fst devs)) in
  List.map (fun id ->
      id, (List.map snd (List.filter (fun (x, _) -> x = id) devs))
    ) ids

let of_string dev =
  Scanf.sscanf dev slash_bdf_scan_fmt (fun id a b c d -> (id, (a, b, c, d)))

let to_string (id, (a, b, c, d)) =
  Printf.sprintf slash_bdf_prnt_fmt id a b c d

let other_pcidevs_of_vm ~__context other_config =
  let devs =
    try
      let oc = List.assoc "pci" other_config in
      String.split ',' oc
    with Not_found -> []
  in
  List.fold_left (fun acc dev ->
      try
        of_string dev :: acc
      with _ -> acc
    ) [] devs

let pci_hiding_key = "xen-pciback.hide"
let pci_hiding_key_eq = pci_hiding_key ^ "="

let get_pci_hidden_raw_value () =
  let cmd = !Xapi_globs.xen_cmdline_path ^ " --get-dom0 " ^ pci_hiding_key in
  let raw_kv_string = Helpers.get_process_output cmd in
  (* E.g. "xen-pciback.hide=(0000:00:02.0)(0000:00:02.1)\n" or just "\n" *)
  if String.startswith pci_hiding_key_eq raw_kv_string then
    let keylen = String.length pci_hiding_key_eq in
    (* rtrim to remove trailing newline *)
    String.rtrim(String.sub_to_end raw_kv_string keylen)
  else
    ""

let get_hidden_pcidevs () =
  let paren_len = String.length "(0000:00:00.0)" in
  let rec read_dev devs raw =
    match raw with
    | "" -> devs
    | _ -> (
        let dev = Scanf.sscanf
            raw bdf_paren_scan_fmt (fun a b c d -> (a, b, c, d)) in
        read_dev (dev::devs) (String.sub_to_end raw paren_len)
      )
  in
  read_dev [] (get_pci_hidden_raw_value ())

let _is_pci_hidden ~__context pci =
  let pcidev = pcidev_of_pci ~__context pci in
  List.mem pcidev (get_hidden_pcidevs ())

(** Check whether a PCI device will be hidden from the dom0 kernel on boot. *)
let is_pci_hidden ~__context pci =
  Mutex.execute m (fun () ->
      _is_pci_hidden ~__context pci
    )

let _hide_pci ~__context pci =
  if not (_is_pci_hidden ~__context pci) then (
    let paren_of (a, b, c, d) = (
      Printf.sprintf bdf_paren_prnt_fmt a b c d
    ) in
    let p = pcidev_of_pci ~__context pci in
    let devs = p::(get_hidden_pcidevs ()) in
    let valstr = List.fold_left (fun acc d -> acc ^ (paren_of d)) "" devs in
    let cmd = Printf.sprintf "%s --set-dom0 %s%s"
        !Xapi_globs.xen_cmdline_path pci_hiding_key_eq valstr in
    let _ = Helpers.get_process_output cmd in
    ()
  )

(** Hide a PCI device from the dom0 kernel. (Takes effect after next boot.) *)
let hide_pci ~__context pci =
  Mutex.execute m (fun () ->
      _hide_pci ~__context pci
    )

let _unhide_pci ~__context pci =
  if (_is_pci_hidden ~__context pci) then (
    let raw_value = get_pci_hidden_raw_value () in
    let bdf_paren = Printf.sprintf "(%s)"
        (Db.PCI.get_pci_id ~__context ~self:pci) in
    let new_value = String.replace bdf_paren "" raw_value in
    let cmd = match new_value with
      | "" -> Printf.sprintf "%s --delete-dom0 %s"
                !Xapi_globs.xen_cmdline_path pci_hiding_key
      | _ -> Printf.sprintf "%s --set-dom0 %s%s"
               !Xapi_globs.xen_cmdline_path pci_hiding_key_eq new_value
    in
    let _ = Helpers.get_process_output cmd in
    ()
  )

(** Unhide a PCI device from the dom0 kernel. (Takes effect after next boot.) *)
let unhide_pci ~__context pci =
  Mutex.execute m (fun () ->
      _unhide_pci ~__context pci
    )

(** Return the id of a PCI device *)
let id_of (id, (domain, bus, dev, fn)) = id

(** Return the domain of a PCI device *)
let domain_of (id, (domain, bus, dev, fn)) = domain

(** Return the bus of a PCI device *)
let bus_of (id, (domain, bus, dev, fn)) = bus

(** Return the device of a PCI device *)
let dev_of (id, (domain, bus, dev, fn)) = dev

(** Return the function of a PCI device *)
let fn_of (id, (domain, bus, dev, fn)) = fn

(** Find a free virtual function given a physical function (SR-IOV) *)
let reserve_free_virtual_function ~__context vm pf =
  let rec search = function
    | [] -> None
    | (vf, _) :: vfs ->
      let attached = Db.PCI.get_attached_VMs ~__context ~self:vf <> [] in
      let scheduled = Db.PCI.get_scheduled_to_be_attached_to ~__context ~self:vf <> Ref.null in
      if attached || scheduled then
        search vfs
      else begin
        Db.PCI.set_scheduled_to_be_attached_to ~__context ~self:vf ~value:vm;
        Some vf
      end
  in
  Db.PCI.get_virtual_functions ~__context ~self:pf
  |> List.map (fun vf -> vf, pcidev_of_pci ~__context vf)
  |> List.sort (fun (_, a) (_, b) -> compare a b)  (* prefer low BDF numbers *)
  |> search
