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
(**
 * @group Pool Management
*)

module Mutex = Xapi_stdext_threads.Threadext.Mutex
module Unixext = Xapi_stdext_unix.Unixext

module D = Debug.Make (struct let name = "pool_role" end)

open D

(** The role of this node *)
type t =
  | Coordinator
  | Supporter of string
  (* IP address *)
  | Broken

let role = ref None

let role_unit_tests = ref false

let role_m = Mutex.create ()

let with_pool_role_lock f = Mutex.execute role_m f

let set_pool_role_for_test () =
  with_pool_role_lock (fun _ ->
      role := Some Coordinator ;
      role_unit_tests := true
  )

let is_unit_test () = with_pool_role_lock (fun _ -> !role_unit_tests)

let string_of = function
  | Coordinator ->
      "master"
  | Supporter x ->
      "slave:" ^ x
  | Broken ->
      "broken"

let read_pool_role () =
  try
    let s =
      Astring.String.trim (Unixext.string_of_file !Constants.pool_config_file)
    in
    match Astring.String.cuts ~sep:":" s with
    | ["master"] ->
        Coordinator
    | "slave" :: m_ip ->
        Supporter (String.concat ":" m_ip)
    | ["broken"] ->
        Broken
    | _ ->
        failwith
          (Printf.sprintf "cannot parse pool_role '%s' from pool config file" s)
  with _ ->
    (* If exec name is suite.opt, we're running as unit tests *)
    if "xapi" <> Filename.basename Sys.executable_name then (
      debug
        "Executable name is not 'xapi', so we must be running in unit-test \
         mode; setting pool-role to 'Coordinator'" ;
      Coordinator
    ) else (
      error "Failed to read pool role from %s" !Constants.pool_config_file ;
      Broken
    )

let get_role () =
  with_pool_role_lock (fun _ ->
      match !role with
      | Some x ->
          x
      | None ->
          let r = read_pool_role () in
          role := Some r ;
          r
  )

let is_coordinator () = get_role () = Coordinator

let is_supporter () = match get_role () with Supporter _ -> true | _ -> false

let is_broken () = get_role () = Broken

exception This_host_is_coordinator

exception This_host_is_broken

let get_address_of_coordinator_exn () =
  match get_role () with
  | Supporter ip ->
      ip
  | Coordinator ->
      raise This_host_is_coordinator
  | Broken ->
      raise This_host_is_broken

let get_address_of_coordinator () =
  match get_role () with
  | Supporter ip ->
      Some ip
  | Coordinator | Broken ->
      None
