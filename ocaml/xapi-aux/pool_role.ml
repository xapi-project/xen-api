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
type t = Master | Slave of string | Broken

let role = ref None

let role_unit_tests = ref false

let role_m = Mutex.create ()

let with_pool_role_lock f = Mutex.execute role_m f

let set_pool_role_for_test () =
  with_pool_role_lock (fun _ ->
      role := Some Master ;
      role_unit_tests := true)

let is_unit_test () = with_pool_role_lock (fun _ -> !role_unit_tests)

let string_of = function
  | Master ->
      "master"
  | Slave x ->
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
        Master
    | "slave" :: m_ip ->
        Slave (String.concat ":" m_ip)
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
         mode; setting pool-role to 'Master'" ;
      Master
    ) else (
      error "Failed to read pool role from %s" !Constants.pool_config_file ;
      Broken
    )

let get_role_without_lock () =
  match !role with
  | Some x ->
      x
  | None ->
      let r = read_pool_role () in
      role := Some r ;
      r

let get_role () = with_pool_role_lock get_role_without_lock

let set_role_for_next_boot r =
  with_pool_role_lock @@ fun () ->
  let current = get_role_without_lock () in
  Unixext.write_string_to_file !Constants.pool_config_file (string_of r) ;
  current

let is_master () = get_role () = Master

let is_slave () = match get_role () with Slave _ -> true | _ -> false

let is_broken () = get_role () = Broken

exception This_host_is_a_master

exception This_host_is_broken

let get_master_address () =
  match get_role () with
  | Slave addr ->
      addr
  | Master ->
      raise This_host_is_a_master
  | Broken ->
      raise This_host_is_broken
