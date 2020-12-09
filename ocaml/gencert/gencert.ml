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

(* Usage: gencert <path> *)

let inventory = "/etc/xensource-inventory"

module Lib = Gencertlib.Lib

module D = Debug.Make (struct let name = "gencert" end)

let generate_cert_or_fail ~path ~cn ~sans ~ip =
  let _ = Gencertlib.Selfcert.generate cn sans path ip in
  if Sys.file_exists path then
    D.info "file exists (%s), assuming cert was created" path
  else (
    D.error "file doesn't exist (%s): cert was not created" path ;
    exit 1
  )

let main ~dbg ~path =
  let init_inventory () = Inventory.inventory_filename := inventory in
  init_inventory () ;
  let ip =
    match Lib.get_management_ip_addr ~dbg with
    | None ->
        D.error "gencert.ml: cannot get management ip address!" ;
        exit 1
    | Some x ->
        x
  in
  let sans =
    match Lib.hostnames () with
    | [] ->
        D.error "could not find any hostnames" ;
        exit 1
    | xs ->
        xs
  in
  let cn = ip in
  generate_cert_or_fail ~path ~cn ~sans ~ip

let () =
  let program_name = Sys.argv.(0) in
  let dbg = Printf.sprintf "%s - %f" program_name (Unix.gettimeofday ()) in
  (* if necessary use Unix.localtime to debug *)
  D.debug "%s" dbg ;
  match Sys.argv with
  | [|_; path|] when Sys.file_exists path ->
      D.info "file already exists at path (%s) - doing nothing" path ;
      exit 0
  | [|_; path|] ->
      main ~dbg ~path
  | _ ->
      D.error "Usage: %s PATH" program_name ;
      exit 1
