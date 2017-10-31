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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

open Cluster_interface

module D=Debug.Make(struct let name="xapi_clustering" end)
open D

(* host-local clustering lock *)
let clustering_lock_m = Mutex.create ()

let with_clustering_lock f =
  debug "Trying to grab host-local clustering lock...";
  Stdext.Threadext.Mutex.execute clustering_lock_m
    (fun () ->
       Stdext.Pervasiveext.finally
         (fun () ->
            debug "Grabbed host-local clustering lock; executing function...";
            f ())
         (fun () -> debug "Function execution finished; returned host-local clustering lock."))

let pif_of_host ~__context network host =
  debug "Looking up PIF for network %s" (Ref.string_of network);
  let pifs = Db.PIF.get_records_where ~__context
      ~expr:Db_filter_types.(And (Eq(Literal (Ref.string_of host),Field "host"),
                                  Eq(Literal (Ref.string_of network),Field "network"))) in
  match pifs with
  | [(ref, record)] ->
    (ref, record)
  | _ ->
    let msg = Printf.sprintf "No PIF found for host:%s and network:%s" (Ref.string_of host) (Ref.string_of network) in
    debug "%s" msg;
    failwith msg

let ip_of_pif (ref,record) =
  let ip = record.API.pIF_IP in
  if ip = "" then failwith (Printf.sprintf "PIF %s does not have any IP" (Ref.string_of ref));
  debug "Got IP %s for PIF %s" ip (Ref.string_of ref);
  Cluster_interface.IPv4 ip

let assert_pif_permaplugged (ref,record) =
  if not record.API.pIF_disallow_unplug then failwith (Printf.sprintf "PIF %s allows unplug" (Ref.string_of ref));
  if not record.pIF_currently_attached then failwith (Printf.sprintf "PIF %s not plugged" (Ref.string_of ref))

let handle_error error =
  (* TODO: replace with API errors? *)
  match error with
  | InternalError message -> failwith ("Internal Error: " ^ message)
  | Unix_error message -> failwith ("Unix Error: " ^ message)

let assert_cluster_host_can_be_created ~__context ~host =
  if Db.Cluster_host.get_refs_where ~__context
      ~expr:Db_filter_types.(Eq(Literal (Ref.string_of host),Field "host")) <> [] then
    failwith "Cluster host cannot be created because it already exists"
