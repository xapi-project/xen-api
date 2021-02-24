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
open Xapi_stdext_threads.Threadext
open Helper_process
module Net = Network_client.Client

let filter_newline s =
  let l = String.length s in
  let rec count_newlines i =
    if i = 0 then
      0
    else
      let chr = s.[i] in
      if chr = '\n' || chr = '\r' then
        count_newlines (i - 1)
      else
        i
  in
  let newline_end = count_newlines (l - 1) in
  String.sub s 0 (newline_end + 1)

let _cached_hostname = ref ""

let _cached_hostname_m = Mutex.create ()

let get_hostname () =
  Mutex.execute _cached_hostname_m (fun () ->
      ( if !_cached_hostname = "" then
          _cached_hostname :=
            try filter_newline (get_process_output "/bin/hostname")
            with _ -> "unknown"
      ) ;
      !_cached_hostname)

(* Fetch the hostname again in case it has changed beneath us *)
let reget_hostname () =
  Mutex.execute _cached_hostname_m (fun () -> _cached_hostname := "") ;
  get_hostname ()

exception Unexpected_address_type of string

(* Try to get all FQDNs, use the hostname if none are available *)
let hostnames () =
  let hostname = Unix.gethostname () in
  let fqdns =
    Unix.getaddrinfo hostname "" [Unix.AI_CANONNAME]
    |> List.map (fun x -> x.Unix.ai_canonname)
  in
  hostname :: fqdns
  |> List.filter_map (fun x ->
         let x = Astring.String.trim x in
         if
           String.equal "" x
           || String.equal "localhost" x
           || Ipaddr.of_string x |> Stdlib.Result.is_ok
         then
           None
         else
           Some x)
  |> Astring.String.uniquify

let get_management_ip_addr ~dbg =
  let iface = Inventory.lookup Inventory._management_interface in
  try
    if iface = "" || (not @@ Net.Interface.exists dbg iface) then
      None
    else
      let addrs =
        match
          String.lowercase_ascii
            (Inventory.lookup Inventory._management_address_type
               ~default:"ipv4")
        with
        | "ipv4" ->
            Net.Interface.get_ipv4_addr dbg iface
        | "ipv6" ->
            Net.Interface.get_ipv6_addr dbg iface
        | s ->
            raise
              (Unexpected_address_type
                 (Printf.sprintf "Expected 'ipv4' or 'ipv6', got %s" s))
      in
      let addrs =
        List.map (fun (addr, _) -> Unix.string_of_inet_addr addr) addrs
      in
      (* Filter out link-local addresses *)
      let addrs =
        List.filter (fun addr -> String.sub addr 0 4 <> "fe80") addrs
      in
      List.nth_opt addrs 0
  with _ -> None
