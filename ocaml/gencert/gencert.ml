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

let inventory = "/etc/xensource-inventory"

module Lib = Gencertlib.Lib

module D = Debug.Make (struct let name = "gencert" end)

(* There are two types of certificates that can be generated, each one is
   served depending on the SNI of the petition being received *)
module SNI = struct
  type t = Default | Xapi_pool

  let of_string = function
    | "default" ->
        Some Default
    | "xapi:pool" ->
        Some Xapi_pool
    | _ ->
        None
end

let generate_cert_or_fail ~generator ~path =
  generator path ;
  if Sys.file_exists path then
    D.info "file exists (%s), assuming cert was created" path
  else (
    D.error "file doesn't exist (%s): cert was not created" path ;
    exit 1
  )

let main ~dbg ~path ~sni =
  let init_inventory () = Inventory.inventory_filename := inventory in
  init_inventory () ;
  let generator path =
    match sni with
    | SNI.Default ->
        let name, ip =
          match Networking_info.get_management_ip_addr ~dbg with
          | None ->
              D.error "gencert.ml: cannot get management ip address!" ;
              exit 1
          | Some x ->
              x
        in
        let dns_names = Networking_info.dns_names () in
        let ips = [ip] in
        let (_ : X509.Certificate.t) =
          Gencertlib.Selfcert.host ~name ~dns_names ~ips path
        in
        ()
    | SNI.Xapi_pool ->
        let uuid = Inventory.lookup Inventory._installation_uuid in
        Gencertlib.Selfcert.xapi_pool ~uuid path
  in
  generate_cert_or_fail ~generator ~path

let () =
  let program_name = Sys.argv.(0) in
  let dbg = Printf.sprintf "%s - %f" program_name (Unix.gettimeofday ()) in
  (* if necessary use Unix.localtime to debug *)
  D.debug "%s" dbg ;
  match Sys.argv with
  | [|_; path; _|] when Sys.file_exists path ->
      D.info "file already exists at path (%s) - doing nothing" path ;
      exit 0
  | [|_; path; sni|] -> (
    match SNI.of_string sni with
    | Some sni ->
        main ~dbg ~path ~sni
    | None ->
        D.error "SNI must be default or xapi:pool, but got '%s'" sni ;
        exit 1
  )
  | _ ->
      D.error "Usage: %s PATH (default|xapi:pool)" program_name ;
      exit 1
