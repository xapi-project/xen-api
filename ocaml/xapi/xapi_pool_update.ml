(*
 * Copyright (C) 2006-2016 Citrix Systems Inc.
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

open Stdext
open Pervasiveext
open Xstringext
open Http
open Forkhelpers
open Xml
open Helpers
open Listext

module D = Debug.Make(struct let name="xapi" end)
open D
(** Updates contain their own metadata in XML format. When the signature has been verified
    the update is executed with argument "info" and it emits XML like the following:

      <info  uuid="foo-bar-baz"
             version="1.0"
             name-label="My First Update(TM)"
             name-description="This is a simple executable update file used for testing"
             after-apply-guidance="restartHVM restartPV restartHost"
      />
*)
type update_info = {
  uuid: string;
  name_label: string;
  name_description: string;
  installation_size: int64;
  after_apply_guidance: API.after_apply_guidance list;
  vdi: API.ref_VDI;
  hosts: API.ref_host list
}

exception Missing_update_key of string
exception Bad_update_info
exception Invalid_update_uuid of string

let check_unsigned_update_fist path =
  match Xapi_fist.allowed_unsigned_updates () with
  | None -> false
  | Some fist ->
    let sha1 =
      Sha1sum.sha1sum (fun checksum_fd ->
          let (_: int64) = Unixext.with_file path [ Unix.O_RDONLY ] 0 (fun fd ->
              Unixext.copy_file fd checksum_fd
            ) in
          ()
        )
    in
    debug "Patch Sha1sum: %s" sha1;
    let fist_sha1s = String.split_f String.isspace fist in
    debug "FIST allowed_unsigned_updates: %s" fist;
    List.mem sha1 fist_sha1s

let assert_space_available ?(multiplier=3L) update_size =
  let open Unixext in
  ignore (Unixext.mkdir_safe Xapi_globs.host_update_dir 0o755);
  let stat = statvfs Xapi_globs.host_update_dir in
  let free_bytes =
    (* block size times free blocks *)
    Int64.mul stat.f_frsize stat.f_bfree in
  let really_required = Int64.mul multiplier update_size in
  if really_required > free_bytes
  then
    begin
      error "Not enough space on filesystem to upload update. Required %Ld, \
             but only %Ld available" really_required free_bytes;
      raise (Api_errors.Server_error (Api_errors.out_of_space, [Xapi_globs.host_update_dir]))
    end

let create_update_record ~__context update_info =
  let r = Ref.make () in
  Db.Pool_update.create ~__context
    ~ref:r
    ~uuid:update_info.uuid
    ~name_label:update_info.name_label
    ~name_description:update_info.name_description
    ~installation_size:update_info.installation_size
    ~after_apply_guidance:update_info.after_apply_guidance
    ~vdi:update_info.vdi;
  r

let load_update_info_from_xml ~__context filename =
  try
    { uuid = "XXXXXXXX-XXXX-4XXX-YXXX-XXXXXXXXXXXX";
      name_label = "XS70E001";
      name_description = "First update for XenServer Ely";
      installation_size = 1L;
      after_apply_guidance = [`restartXAPI];
      vdi = List.hd (Db.VDI.get_all ~__context);
      hosts = [];
    }
  with
  | _ -> raise Bad_update_info

exception Cannot_expose_yum_repo_on_slave

let pool_update_yum_repo_handler (req: Request.t) s _ =
  debug "Update yum repo - Entered...";

  if not (Pool_role.is_master ())
  then raise Cannot_expose_yum_repo_on_slave;

  Xapi_http.with_context "Update yum repo" req s
    (fun __context ->
       raise (Api_errors.Server_error (Api_errors.not_implemented, [ "pool_update_yum_repo_handler" ]))
    )

let introduce ~__context ~vdi =
  let vdi_name = Db.VDI.get_name_label ~__context ~self:vdi in
  let update_info = load_update_info_from_xml ~__context vdi_name in
  create_update_record ~__context update_info

let pool_apply ~__context ~self =
  let pool_update_name = Db.Pool_update.get_name_label ~__context ~self in
  debug "pool_update.pool_apply %s" pool_update_name;
  ()

let clean ~__context ~self ~host =
  let pool_update_name = Db.Pool_update.get_name_label ~__context ~self in
  let host_name = Db.Host.get_name_label ~__context ~self:host in
  debug "pool_update.clean %s on %s" pool_update_name host_name;
  ()

let pool_clean ~__context ~self =
  let pool_update_name = Db.Pool_update.get_name_label ~__context ~self in
  debug "pool_update.pool_clean %s" pool_update_name;
  ()

let destroy ~__context ~self =
  let pool_update_name = Db.Pool_update.get_name_label ~__context ~self in
  debug "pool_update.destroy %s" pool_update_name;
  ()
