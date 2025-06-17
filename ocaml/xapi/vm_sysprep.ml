(*
 * Copyright (c) Cloud Software Group, Inc.
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

module D = Debug.Make (struct let name = __MODULE__ end)

open D
open Xapi_stdext_unix

let ( // ) = Filename.concat

let finally = Xapi_stdext_pervasives.Pervasiveext.finally

let genisoimage = "/usr/bin/genisoimage"

let failwith_fmt fmt = Printf.ksprintf failwith fmt

let prng = Random.State.make_self_init ()

module SR = struct
  let dir = "/var/opt/iso"

  let name hostname = Printf.sprintf "SYSPREP-%s" hostname
end

(** create a name with a random infix *)
let temp_name prefix suffix =
  let rnd = Random.State.bits prng land 0xFFFFFF in
  Printf.sprintf "%s%06x%s" prefix rnd suffix

let temp_dir = Filename.get_temp_dir_name ()

(** [mkdtmp] creates a directory in [dir] and returns its path. If [dir]
    does not yet exist it is created. It is a an error if [dir] exists
    and is not a directory. *)
let mkdtemp ?(dir = temp_dir) ?(perms = 0o700) prefix suffix =
  ( match Sys.file_exists dir with
  | true when not (Sys.is_directory dir) ->
      failwith_fmt "s: %s is not a directory" __FUNCTION__ dir
  | true ->
      ()
  | false ->
      Unixext.mkdir_rec dir perms
  ) ;
  let rec loop = function
    | n when n >= 20 ->
        failwith_fmt "s: can't create directory in %s" __FUNCTION__ dir
    | n -> (
        let path = Filename.concat dir (temp_name prefix suffix) in
        try Sys.mkdir path perms ; path with Sys_error _ -> loop (n + 1)
      )
  in
  loop 0

(** Crteate a temporary directory, and pass its path to [f]. Once [f]
    returns the directory is removed again *)
let with_temp_dir ?(dir = temp_dir) ?(perms = 0o700) prefix suffix f =
  let dir = mkdtemp ~dir ~perms prefix suffix in
  finally (fun () -> f dir) (fun () -> Unixext.rm_rec dir)

(** name of the ISO we will use for a VM; this is not a path *)
let iso_basename ~vm_uuid =
  let now = Ptime_clock.now () |> Ptime.to_rfc3339 in
  Printf.sprintf "sysprep-%s-%s.iso" vm_uuid now

(** Create an ISO in [SR.dir] with content [unattend]. [SR.dir] is
    created if it not already exists. Returns the path of the ISO image *)
let make_iso ~vm_uuid ~unattend =
  try
    let basename = iso_basename ~vm_uuid in
    let iso = SR.dir // basename in
    Xapi_stdext_unix.Unixext.mkdir_rec SR.dir 0o755 ;
    with_temp_dir ~dir:"/var/tmp/xapi" "sysprep-" "-iso" (fun temp_dir ->
        let path = temp_dir // "unattend.xml" in
        Unixext.write_string_to_file path unattend ;
        debug "%s: written to %s" __FUNCTION__ path ;
        let args = ["-r"; "-J"; "-o"; iso; temp_dir] in
        Forkhelpers.execute_command_get_output genisoimage args |> ignore ;
        (iso, basename)
    )
  with e ->
    let msg = Printexc.to_string e in
    Helpers.internal_error "%s failed: %s" __FUNCTION__ msg

(** create a local ISO SR when necessary and update it such that it
    recognises any ISO we added or removed *)
let update_sr ~__context =
  let host = Helpers.get_localhost ~__context in
  let hostname = Db.Host.get_hostname ~__context ~self:host in
  let label = SR.name hostname in
  let mib n = Int64.(n * 1024 * 1024 |> of_int) in
  let sr =
    match Db.SR.get_by_name_label ~__context ~label with
    | [sr] ->
        sr
    | sr :: _ ->
        warn "%s: more than one SR with label %s" __FUNCTION__ label ;
        sr
    | [] ->
        let device_config = [("location", SR.dir); ("legacy_mode", "true")] in
        Xapi_sr.create ~__context ~host ~name_label:label ~device_config
          ~content_type:"iso" ~_type:"iso" ~name_description:"Sysprep ISOs"
          ~shared:false ~sm_config:[] ~physical_size:(mib 512)
  in
  Xapi_sr.scan ~__context ~sr ;
  sr

(** Find the VBD for the CD drive on [vm] *)
let find_cdr_vbd ~__context ~vm =
  let vbds = Db.VM.get_VBDs ~__context ~self:vm in
  let vbds' =
    List.map (fun self -> (self, Db.VBD.get_record ~__context ~self)) vbds
  in
  let is_cd (_rf, rc) =
    let open API in
    rc.vBD_type = `CD && rc.vBD_empty
  in
  let uuid = Db.VM.get_uuid ~__context ~self:vm in
  match List.filter is_cd vbds' with
  | [] ->
      failwith_fmt "%s: can't find CDR for VM %s" __FUNCTION__ uuid
  | [(rf, rc)] ->
      debug "%s: for VM %s using VBD %s" __FUNCTION__ uuid rc.API.vBD_uuid ;
      rf
  | (rf, rc) :: _ ->
      debug "%s: for VM %s using VBD %s" __FUNCTION__ uuid rc.API.vBD_uuid ;
      warn "%s: for VM %s found additions VBDs" __FUNCTION__ uuid ;
      rf

(** Find the VDI that contains the unattend.xml based on its name. This
    should be unique *)
let find_vdi ~__context ~label =
  match Db.VDI.get_by_name_label ~__context ~label with
  | [] ->
      failwith_fmt "%s: can't find VDI for %s" __FUNCTION__ label
  | [vdi] ->
      vdi
  | vdi :: _ ->
      warn "%s: more than one VDI with label %s" __FUNCTION__ label ;
      vdi

let trigger ~domid =
  let open Ezxenstore_core.Xenstore in
  let control = Printf.sprintf "/local/domain/%Ld/control/sysprep" domid in
  with_xs (fun xs ->
      xs.Xs.write (control // "filename") "D://unattend.xml" ;
      Thread.delay 5.0 ;
      xs.Xs.write (control // "action") "sysprep"
  ) ;
  debug "%s: notified domain %Ld" __FUNCTION__ domid

(* This function is executed on the host where [vm] is running *)
let sysprep ~__context ~vm ~unattend =
  debug "%s" __FUNCTION__ ;
  let vm_uuid = Db.VM.get_uuid ~__context ~self:vm in
  let domid = Db.VM.get_domid ~__context ~self:vm in
  if domid <= 0L then
    failwith_fmt "%s: VM %s does not have a domain" __FUNCTION__ vm_uuid ;
  let iso, label = make_iso ~vm_uuid ~unattend in
  debug "%s: created ISO %s" __FUNCTION__ iso ;
  let _sr = update_sr ~__context in
  let vbd = find_cdr_vbd ~__context ~vm in
  let vdi = find_vdi ~__context ~label in
  debug "%s: inserting Sysppep VDI for VM %s" __FUNCTION__ vm_uuid ;
  Xapi_vbd.insert ~__context ~vdi ~vbd ;
  Thread.delay 5.0 ;
  trigger ~domid
