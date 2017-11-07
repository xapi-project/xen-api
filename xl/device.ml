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

open Printf
open Xenops_utils
open Xenstore

module D = Debug.Make(struct let name = "xenops" end)
open D

module Generic = struct
  let vnc_port_path domid = sprintf "/local/domain/%d/console/vnc-port" domid
  let tc_port_path domid = sprintf "/local/domain/%d/console/tc-port" domid

  let best_effort txt f =
    try
      f ()
    with e ->
      info "%s: ignoring exception %s" txt (Printexc.to_string e)
end

module PV_Vnc = struct
  let vnc_pid_path domid = sprintf "/local/domain/%d/vncterm-pid" domid
  let vnc_console_path domid = sprintf "/local/domain/%d/console" domid

  let pid ~xs domid =
    try
      let pid = xs.Xs.read (vnc_pid_path domid) in
      Some (int_of_string pid)
    with _ ->
      None

  (* Look up the commandline args for the vncterm pid; *)
  (* Check that they include the vncterm binary path and the xenstore console path for the supplied domid. *)
  let is_cmdline_valid domid pid =

    let is_null = function | '\000' -> true | _ -> false in
    let cmdline =
      Printf.sprintf "/proc/%d/cmdline" pid
      |> Stdext.Unixext.string_of_file
      |> Stdext.Xstringext.String.split_f is_null
    in
    if (List.mem !Xl_resources.vncterm cmdline) && (List.mem (vnc_console_path domid) cmdline)
    then true
    else false

  let is_vncterm_running ~xs domid =
    match pid ~xs domid with
    | None -> false
    | Some p ->
      try
        Unix.kill p 0;
        is_cmdline_valid domid p
      with _ -> false

  let get_vnc_port ~xs domid =
    if not (is_vncterm_running ~xs domid)
    then None
    else (try Some(int_of_string (xs.Xs.read (Generic.vnc_port_path domid))) with _ -> None)

  let get_tc_port ~xs domid =
    if not (is_vncterm_running ~xs domid)
    then None
    else (try Some(int_of_string (xs.Xs.read (Generic.tc_port_path domid))) with _ -> None)

  let load_args = function
    | None -> []
    | Some filename ->
      if Sys.file_exists filename
      then ["-l"; filename]
      else []

  exception Failed_to_start

  let start ?statefile ~xs ?ip domid =
    debug "In PV_Vnc.start";
    let ip = Opt.default "127.0.0.1" ip in
    let l = [ "-x"; sprintf "/local/domain/%d/console" domid;
              "-T"; (* listen for raw connections *)
              "-v"; ip ^ ":1";
            ] @ load_args statefile in
    (* Now add the close fds wrapper *)
    let pid = Forkhelpers.safe_close_and_exec None None None [] !Xl_resources.vncterm l in
    let path = vnc_pid_path domid in
    xs.Xs.write path (string_of_int (Forkhelpers.getpid pid));
    Forkhelpers.dontwaitpid pid

  let stop ~xs domid =
    let open Generic in
    match pid ~xs domid with
    | Some pid ->
      best_effort "killing vncterm"
        (fun () -> Unix.kill (-pid) Sys.sigterm);
      best_effort "removing vncterm-pid from xenstore"
        (fun () -> xs.Xs.rm (vnc_pid_path domid))
    | None -> ()
end

module Qemu = struct
  (* Where qemu-dm-wrapper writes its pid *)
  let qemu_pid_path domid = sprintf "/local/domain/%d/qemu-pid" domid

  let pid ~xs domid =
    try
      let pid = xs.Xs.read (qemu_pid_path domid) in
      Some (int_of_string pid)
    with _ ->
      None

  let is_running ~xs domid =
    match pid ~xs domid with
    | None -> false
    | Some p ->
      try
        Unix.kill p 0;
        true
      with _ -> false
end

module Dm = struct
  let get_vnc_port ~xs domid =
    if false (*not (Qemu.is_running ~xs domid)*)
    then None
    else (try Some(int_of_string (xs.Xs.read (Generic.vnc_port_path domid))) with _ -> None)

  let get_tc_port ~xs domid =
    if not (Qemu.is_running ~xs domid)
    then None
    else (try Some(int_of_string (xs.Xs.read (Generic.tc_port_path domid))) with _ -> None)
end

let get_vnc_port ~xs domid = 
  (* Check whether a qemu exists for this domain *)
  (*	let qemu_exists = Qemu.is_running ~xs domid in *)
  if true (* qemu_exists *)
  then Dm.get_vnc_port ~xs domid
  else PV_Vnc.get_vnc_port ~xs domid

let get_tc_port ~xs domid = 
  (* Check whether a qemu exists for this domain *)
  let qemu_exists = Qemu.is_running ~xs domid in
  if qemu_exists
  then Dm.get_tc_port ~xs domid
  else PV_Vnc.get_tc_port ~xs domid

