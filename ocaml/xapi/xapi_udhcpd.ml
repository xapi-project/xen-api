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
(* Interface to udhcpd *)
open Stdext
open Xstringext

module D = Debug.Make(struct let name="xapi_udhcpd" end)
open D

open Forkhelpers
open Pervasiveext
open Threadext

let ip_begin_key = "ip_begin"
let ip_end_key = "ip_end"
let ip_disable_gw_key = "ip_disable_gw"
let pxe_server_key = "pxe_server"

module Ip = struct
  type t = int * int * int * int [@@deriving rpc]

  exception Invalid_ip of t

  let check ((a, b, c, d) as ip) =
    if a >= 256 || b >= 256 || c >= 256 || d >=256 then raise (Invalid_ip ip) else ip

  let string_of (a, b, c, d) = Printf.sprintf "%d.%d.%d.%d" a b c d
  let of_string s = Scanf.sscanf s "%d.%d.%d.%d" (fun a b c d -> check (a,b,c,d))

  (** [succ ip] returns the "next" address after [ip] *)
  let succ (a, b, c, d) =
    let (a, b, c, d) = (a, b, c, d + 1) in
    let (a, b, c, d) = if d < 256 then (a, b, c, d) else (a, b, c + 1, 0) in
    let (a, b, c, d) = if c < 256 then (a, b, c, d) else (a, b + 1, 0, d) in
    let (a, b, c, d) = if b < 256 then (a, b, c, d) else (a + 1, 0, c, d) in
    check (a, b, c, d)

  (** [gt a b] returns true iff [a] is later than [b] in the sequence *)
  let gt (a, b, c, d) (a', b', c', d') =
    (a > a') || ((a = a') && (b > b')) || ((a = a') && (b = b') && (c > c')) || ((a = a') && (b = b') && (c = c') && (d > d'))

  (** [first a b f] returns [Some x] where [x] is the first address in the sequence from
      		[a] to [b] where [f x] is true if it exists, and [None] otherwise. *)
  let rec first a b f =
    if gt a b then None
    else
    if f a then Some a
    else first (succ a) b f
end

type static_lease = {
  mac : string;
  ip : Ip.t;
  vif : string; (* API.ref_VIF *)
  network : string; (* API.ref_network *)
} [@@deriving rpc]

type static_leases = static_lease list [@@deriving rpc]

(** List of static leases. Protected by mutex below. *)
let assigned = ref []

(** Updates the assigned_ips field of networks in xapi's database *)
let update_db_nolock ~__context =
  let loc_assigned = !assigned in
  let networks = List.map (fun lease -> lease.network) loc_assigned |> Listext.List.setify in
  let update_network net =
    let cur_assigned = Db.Network.get_assigned_ips ~__context ~self:(Ref.of_string net) in
    let cur_vifs = List.filter (fun lease -> lease.network = net) loc_assigned
                   |> List.map (fun l -> Ref.of_string l.vif) in
    let db_vifs = List.map fst cur_assigned in
    let new_lease_vifs = Listext.List.set_difference cur_vifs db_vifs in
    let released_lease_vifs = Listext.List.set_difference db_vifs cur_vifs in
    List.iter (fun new_lease_vif ->
        let lease =
          List.find (fun x -> x.vif = Ref.string_of new_lease_vif) loc_assigned in
        Db.Network.add_to_assigned_ips ~__context ~self:(Ref.of_string net)
          ~key:new_lease_vif ~value:(Ip.string_of lease.ip)) new_lease_vifs;
    List.iter (fun released_lease_vif ->
        Db.Network.remove_from_assigned_ips ~__context ~self:(Ref.of_string net) ~key:released_lease_vif
      ) released_lease_vifs
  in
  List.iter update_network networks

(** Called on startup to reload the leases database *)
let load_db_nolock () =
  let s = Unixext.string_of_file !Xapi_globs.udhcpd_leases_db in
  let rpc = Jsonrpc.of_string s in
  assigned := static_leases_of_rpc rpc;
  info "Host internal management network successfully loaded DHCP leases db from %s" !Xapi_globs.udhcpd_leases_db

(** Called before every update to save the leases database *)
let save_db_nolock () =
  let rpc = rpc_of_static_leases !assigned in
  let s = Jsonrpc.to_string rpc in
  Unixext.write_string_to_file !Xapi_globs.udhcpd_leases_db s

module Udhcpd_conf = struct
  type t = {
    interface: string;
    subnet: string;
    router: Ip.t;
    leases: static_leases;
  }

  let make ~__context leases router =
    let network = Helpers.get_guest_installer_network ~__context in
    let interface = Db.Network.get_bridge ~__context ~self:network in
    let other_config = Db.Network.get_other_config ~__context ~self:network in
    let subnet = List.assoc "netmask" other_config in
    {
      interface;
      subnet;
      router;
      leases;
    }

  let to_string ~__context t =
    let skel = Unixext.string_of_file !Xapi_globs.udhcpd_skel in
    let interface = Printf.sprintf "interface\t%s" t.interface in
    let subnet = Printf.sprintf "option\tsubnet\t%s" t.subnet in
    let router = Printf.sprintf "option\trouter\t%s" (Ip.string_of t.router) in
    let pxe = Printf.sprintf "siaddr\t%s\nboot_file\t/pxelinux.0" (Ip.string_of t.router) in
    let string_of_lease l =
      Printf.sprintf "static_lease\t%s\t%s # %s\n" l.mac (Ip.string_of l.ip) l.vif in
    let leases = List.map string_of_lease t.leases in
    let network = Helpers.get_guest_installer_network ~__context in
    let other_config = Db.Network.get_other_config ~__context ~self:network in
    let include_gw =
      try not (List.assoc ip_disable_gw_key other_config = "true")
      with Not_found -> true in
    let include_pxe =
      try List.assoc pxe_server_key other_config = "true"
      with Not_found -> false in
    let config_list =
      skel
      :: interface
      :: subnet
      :: (if include_gw then [router] else [])
      @  (if include_pxe then [pxe] else [])
      @  leases in
    String.concat "\n" config_list
end

let write_config_nolock ~__context ip_router =
  let config = Udhcpd_conf.make ~__context (!assigned) ip_router in
  Unixext.unlink_safe !Xapi_globs.udhcpd_conf;
  Unixext.write_string_to_file !Xapi_globs.udhcpd_conf (Udhcpd_conf.to_string ~__context config)

let restart_nolock () =
  let pid = try Unixext.pidfile_read !Xapi_globs.udhcpd_pidfile with _ -> None in
  Opt.iter Unixext.kill_and_wait pid;
  let (_: string * string) = execute_command_get_output !Xapi_globs.busybox [ "udhcpd"; !Xapi_globs.udhcpd_conf ] in
  let start = Mtime_clock.counter () in
  let rec wait_for_pid n =
    let now = Mtime_clock.count start in
    if Mtime.Span.to_s now > 30.0 then failwith "Failed to start udhcpd";
    let pid = try Unixext.pidfile_read !Xapi_globs.udhcpd_pidfile with _ -> None in
    match pid with
    | Some _ -> ()
    | None ->
      if n>0 && n mod 10 = 0 then debug "Continuing to wait for the pidfile from udhcpd";
      Thread.delay 0.1;
      wait_for_pid (n+1)
  in wait_for_pid 0

let find_lease_nolock vif =
  try Some (List.find (fun l -> l.vif = vif) !assigned)
  with Not_found -> None

(* We only expire leases when the VIFs are *destroyed* from the database. Otherwise
   we get into trouble with sequences like VM.suspend, VM.resume *)
let gc_leases_nolock ~__context =
  let vif_still_exists l = Db.is_valid_ref __context (Ref.of_string l.vif) in
  let good, bad = List.partition vif_still_exists !assigned in
  List.iter (fun l ->
      info "Host internal management network removing lease for VIF %s -> %s" l.vif (Ip.string_of l.ip)
    ) bad;
  assigned := good

let maybe_add_lease_nolock ~__context vif =
  let network = Helpers.get_host_internal_management_network ~__context in
  if network = Db.VIF.get_network ~__context ~self:vif
  then begin
    let other_config = Db.Network.get_other_config ~__context ~self:network in
    if not(List.mem_assoc ip_begin_key other_config) || not(List.mem_assoc ip_end_key other_config)
    then failwith (Printf.sprintf "Host internal management network %s other_config has no ip_begin/ip_end keys" (Ref.string_of network));

    let ip_begin = Ip.of_string (List.assoc ip_begin_key other_config)
    and ip_end = Ip.of_string (List.assoc ip_end_key other_config) in
    match find_lease_nolock (Ref.string_of vif) with
    | Some l ->
      info "VIF %s on host-internal management network already has lease: %s"
        (Ref.string_of vif) (Ip.string_of l.ip);
      restart_nolock ()
    | None ->
      gc_leases_nolock ~__context;
      let mac = Db.VIF.get_MAC ~__context ~self:vif in
      (* NB ip_begin is the address on the bridge itself *)
      begin match Ip.first (Ip.succ ip_begin) ip_end
                    (fun ip -> List.filter (fun l -> l.ip = ip) !assigned = []) with
      | Some ip ->
        assigned := {mac; ip; vif = Ref.string_of vif; network = Ref.string_of network} :: !assigned;
        save_db_nolock ();
        update_db_nolock ~__context;
        write_config_nolock ~__context ip_begin;
        restart_nolock ()
      | None ->
        error "VM on guest installer network, but not IPs available";
        failwith "No IP addresses left"
      end
  end

let mutex = Mutex.create ()

let maybe_add_lease ~__context vif =
  Helpers.log_exn_continue (Printf.sprintf "maybe_add_lease VIF:%s" (Ref.string_of vif)) (fun () ->
      Mutex.execute mutex (fun () ->
          maybe_add_lease_nolock ~__context vif
        )
    ) ()

let get_ip ~__context vif =
  let vif = Ref.string_of vif in
  Mutex.execute mutex (fun () ->
      Opt.map (fun l -> l.ip) (find_lease_nolock vif)
    )

let init () =
  Mutex.execute mutex (fun () ->
      try load_db_nolock ()
      with e ->
        info "Caught exception %s loading %s: creating new empty leases database" (Printexc.to_string e) !Xapi_globs.udhcpd_leases_db;
        assigned := []
    )
