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

let generate_cert_or_fail ~path ~cn =
  let stdout, stderr = Lib.call_generate_ssl_cert ~args:[path; cn] in
  let debug_lines prefix std_x =
    let lines = String.split_on_char '\n' std_x in
    List.iter (fun l -> D.debug {|%s: "%s"|} prefix l) lines
  in
  debug_lines "generate_ssl_cert stdout" stdout ;
  debug_lines "generate_ssl_cert stderr" stderr ;
  if Sys.file_exists path then
    D.info "file exists (%s), assuming cert was created" path
  else (
    D.error "file doesn't exist (%s): cert was not created" path ;
    exit 1
  )

(* Try to get the FQDN, use the hostname if none are available *)
let full_hostname () =
  let hostname = Unix.gethostname () in
  let fqdns =
    Unix.getaddrinfo hostname "" [Unix.AI_CANONNAME]
    |> List.filter_map (fun addrinfo ->
           match addrinfo.Unix.ai_canonname with
           | "" ->
               None
           | name ->
               Some name
       )
  in
  match fqdns with fqdn :: _ -> fqdn | [] -> hostname

let main ~dbg ~path =
  let init_inventory () = Inventory.inventory_filename := inventory in
  init_inventory () ;
  let hostname = full_hostname () in
  let cn =
    if Astring.String.is_prefix ~affix:"localhost" hostname then
      Lib.get_management_ip_addr ~dbg
    else if Astring.String.is_infix ~affix:"." hostname then
      None
    else
      Lib.get_management_ip_addr ~dbg
  in
  let cn = Option.value ~default:hostname cn in
  generate_cert_or_fail ~path ~cn

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
