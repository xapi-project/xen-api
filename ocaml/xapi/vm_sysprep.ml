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

let genisoimage = !Xapi_globs.genisoimage_path

(** This will be shown to the user to explain a failure *)
exception Sysprep of string

let fail fmt = Printf.ksprintf (fun msg -> raise (Sysprep msg)) fmt

let internal_error = Helpers.internal_error

let prng = Random.State.make_self_init ()

(* A local ISO SR; we create an ISO that holds an unattend.xml file that
   is than passed as CD to a VM *)
module SR = struct
  let dir = "/var/opt/iso"

  let name hostname = Printf.sprintf "SYSPREP-%s" hostname

  let find_opt ~__context ~label =
    match Db.SR.get_by_name_label ~__context ~label with
    | [sr] ->
        Some sr
    | sr :: _ ->
        warn "%s: more than one SR with label %s" __FUNCTION__ label ;
        Some sr
    | [] ->
        None
end

(** This is called on xapi startup. Opportunity to set up or clean up.
    We destroy all VDIs that are unused. *)
let on_startup ~__context =
  let host = Helpers.get_localhost ~__context in
  let hostname = Db.Host.get_hostname ~__context ~self:host in
  match SR.find_opt ~__context ~label:(SR.name hostname) with
  | None ->
      ()
  | Some sr -> (
      Db.SR.get_VDIs ~__context ~self:sr
      |> List.iter @@ fun self ->
         match Db.VDI.get_record ~__context ~self with
         | API.{vDI_VBDs= []; vDI_location= _location; _} ->
             Xapi_vdi.destroy ~__context ~self
         | _ ->
             ()
    )

(** create a name with a random infix. We need random names for
    temporay directories to avoid collition *)
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
      internal_error "s: %s is not a directory" __FUNCTION__ dir
  | true ->
      ()
  | false ->
      Unixext.mkdir_rec dir perms
  ) ;
  let rec try_upto = function
    | n when n < 0 ->
        internal_error "%s: can't create directory %S" __FUNCTION__ dir
    | n -> (
        let path = Filename.concat dir (temp_name prefix suffix) in
        try Sys.mkdir path perms ; path with Sys_error _ -> try_upto (n - 1)
      )
  in
  try_upto 20

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
    match SR.find_opt ~__context ~label with
    | Some sr ->
        sr
    | None ->
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
      fail "can't find CDR for VM %s" uuid
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
      internal_error "%s: can't find VDI for %s" __FUNCTION__ label
  | [vdi] ->
      vdi
  | vdi :: _ ->
      warn "%s: more than one VDI with label %s" __FUNCTION__ label ;
      vdi

(** notify the VM with [domid] to run sysprep and where to find the
    file. *)
let trigger ~domid =
  let open Ezxenstore_core.Xenstore in
  let control = Printf.sprintf "/local/domain/%Ld/control/sysprep" domid in
  with_xs (fun xs ->
      xs.Xs.write (control // "filename") "D://unattend.xml" ;
      Thread.delay 5.0 ;
      xs.Xs.write (control // "action") "sysprep" ;
      debug "%s: notified domain %Ld" __FUNCTION__ domid ;
      let rec wait n =
        match (n, xs.Xs.read (control // "action")) with
        | _, "running" ->
            "running"
        | n, action when n < 0 ->
            action
        | _, _ ->
            Thread.delay 1.0 ;
            wait (n - 1)
      in
      (* wait up to 5 iterations for runnung to appear or report whatever
         is the status at the end *)
      wait 5
  )

(* This function is executed on the host where [vm] is running *)
let sysprep ~__context ~vm ~unattend =
  let open Ezxenstore_core.Xenstore in
  debug "%s" __FUNCTION__ ;
  if not !Xapi_globs.vm_sysprep_enabled then
    fail "Experimental VM.sysprep API call is not enabled" ;
  let vm_uuid = Db.VM.get_uuid ~__context ~self:vm in
  let domid = Db.VM.get_domid ~__context ~self:vm in
  let control = Printf.sprintf "/local/domain/%Ld/control" domid in
  if domid <= 0L then
    fail " VM %s is not running" __FUNCTION__ vm_uuid ;
  if String.length unattend > 32 * 1024 then
    fail "%s: provided file for %s larger than 32KiB" __FUNCTION__ vm_uuid ;
  with_xs (fun xs ->
      match xs.Xs.read (control // "feature-sysprep") with
      | "1" ->
          debug "%s: VM %s supports sysprep" __FUNCTION__ vm_uuid
      | _ ->
          fail "VM %s does not support sysprep" vm_uuid
      | exception _ ->
          fail "VM %s does not support sysprep" vm_uuid
  ) ;
  let iso, label = make_iso ~vm_uuid ~unattend in
  debug "%s: created ISO %s" __FUNCTION__ iso ;
  let _sr = update_sr ~__context in
  let vbd = find_cdr_vbd ~__context ~vm in
  let vdi = find_vdi ~__context ~label in
  debug "%s: inserting Sysprep VDI for VM %s" __FUNCTION__ vm_uuid ;
  Xapi_vbd.insert ~__context ~vdi ~vbd ;
  Thread.delay 5.0 ;
  match trigger ~domid with
  | "running" ->
      debug "%s: sysprep running, ejecting CD" __FUNCTION__ ;
      Xapi_vbd.eject ~__context ~vbd ;
      Sys.remove iso
  | status ->
      debug "%s: sysprep %S, ejecting CD" __FUNCTION__ status ;
      Xapi_vbd.eject ~__context ~vbd ;
      Sys.remove iso ;
      fail "VM %s sysprep not found running as expected: %S" vm_uuid status
