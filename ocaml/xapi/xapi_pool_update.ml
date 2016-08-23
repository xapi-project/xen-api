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
open Client

module D = Debug.Make(struct let name="xapi" end)
open D
(** Updates contain their own metadata in XML format. When the signature has been verified
    the update is executed with argument "info" and it emits XML like the following:

      <update
        name-label="XS70E002" uuid="12acfd0a-0246-4fa7-ba00-2d0e474b3650"
        key="test@dev.null.com"
        after-apply-guidance="" guidance-mandatory="false">
        <name-description>Hotfix description</name-description>
      </update>
*)
type update_info = {
  uuid: string;
  name_label: string;
  name_description: string;
  key: string;
  installation_size: int64;
  after_apply_guidance: API.after_apply_guidance list;
}

(** Mount a filesystem somewhere, with optional type *)
let mount ?ty:(ty = None) ?lo:(lo = true) src dest =
  let ty = match ty with None -> [] | Some ty -> [ "-t"; ty ] in
  let lo = if lo then ["-o"; "loop"] else [] in
  ignore(Forkhelpers.execute_command_get_output "/bin/mount" (ty @ lo @ [src; dest ]))

let timeout = 300. (* 5 minutes: something is seriously wrong if we hit this timeout *)
exception Umount_timeout

(** Unmount a mountpoint. Retries every 5 secs for a total of 5mins before returning failure *)
let umount ?(retry=true) dest =
  let finished = ref false in
  let start = Unix.gettimeofday () in

  while not(!finished) && (Unix.gettimeofday () -. start < timeout) do
    try
      ignore(Forkhelpers.execute_command_get_output "/bin/umount" [dest] );
      finished := true
    with e ->
      if not(retry) then raise e;
      debug "Caught exception (%s) while unmounting %s: pausing before retrying"
        (ExnHelper.string_of_exn e) dest;
      Thread.delay 5.
  done;
  if not(!finished) then raise Umount_timeout

let vdi_visible ~__context ~self ~host =
  let vdi = Db.Pool_update.get_vdi ~__context ~self in
  let sr = Db.VDI.get_SR ~__context ~self:vdi in
  let shared = Db.SR.get_shared ~__context ~self:sr in
  let pbds = Db.SR.get_PBDs ~__context ~self:sr in
  let plugged_pbds = List.filter (fun pbd -> Db.PBD.get_currently_attached ~__context ~self:pbd) pbds in
  let plugged_hosts = List.setify (List.map (fun pbd -> Db.PBD.get_host ~__context ~self:pbd) plugged_pbds) in
  (shared = true) || ((List.length pbds = 1) && (List.exists ( fun h -> h = host) plugged_hosts))

let detach ~__context ~self ~host =
  let uuid = Db.Pool_update.get_uuid ~__context ~self in
  let mount_point_parent_dir = String.concat "/" [Xapi_globs.host_update_dir; uuid] in
  let mount_point = Filename.concat mount_point_parent_dir "vdi" in
  let vdi = Db.Pool_update.get_vdi ~__context ~self in
  debug "pool_update.detach %s from %s" (Db.Pool_update.get_name_label ~__context ~self) mount_point;
  if vdi_visible ~__context ~self ~host then begin
    Helpers.log_exn_continue ("detach: unmounting " ^ mount_point)
      (fun () -> umount mount_point) ();
    Helpers.call_api_functions ~__context
      (fun rpc session_id ->
         let dom0 = Helpers.get_domain_zero ~__context in
         let vbds = Client.VDI.get_VBDs ~rpc ~session_id ~self:vdi in
         let vbd = List.find (fun self -> Client.VBD.get_VM ~rpc ~session_id ~self = dom0) vbds in
         Client.VBD.unplug ~rpc ~session_id ~self:vbd;
         Client.VBD.destroy ~rpc ~session_id ~self:vbd
      )
  end;
  let output, _ = Forkhelpers.execute_command_get_output "/bin/rm" ["-r"; mount_point_parent_dir] in
  debug "pool_update.detach Mountpoint removed (output=%s)" output

let with_api_errors f x =
  try f x
  with
  | Smint.Command_failed(ret, status, stdout_log, stderr_log)
  | Smint.Command_killed(ret, status, stdout_log, stderr_log) ->
    let msg = Printf.sprintf "Smint.Command_{failed,killed} ret = %d; status = %s; stdout = %s; stderr = %s"
        ret status stdout_log stderr_log in
    raise (Api_errors.Server_error (Api_errors.internal_error, [msg]))

(* yum confif example
   [main]
   keepcache=0
   reposdir=/dev/null
   gpgcheck=$signed
   repo_gpgcheck=$signed

   [$label]
   name=$label
   baseurl=url
   ${signed:+gpgkey=file:///etc/pki/rpm-gpg/key}
*)
let create_yum_config ~__context ~self ~url ~location =
  let key = Db.Pool_update.get_key ~__context ~self in
  let signed = String.length key <> 0 in
  let signed_index = if signed then 1 else 0 in
  let name_label = Db.Pool_update.get_name_label ~__context ~self in
  let yum_config = location ^ "/yum.conf" in
  let oc = open_out yum_config in
  Printf.fprintf oc "[main]\nkeepcache=0\nreposdir=/dev/null\ngpgcheck=%d\nrepo_gpgcheck=%d\n\n" signed_index signed_index;
  Printf.fprintf oc "[%s]\nname=%s\nbaseurl=%s\n" name_label name_label url;
  if signed then Printf.fprintf oc "gpgkey=file:///etc/pki/rpm-gpg/key";
  close_out oc

let attach ~__context ~self ~host =
  let uuid = Db.Pool_update.get_uuid ~__context ~self in
  let mount_point_parent_dir = String.concat "/" [Xapi_globs.host_update_dir; uuid] in
  let mount_point = Filename.concat mount_point_parent_dir "vdi" in
  debug "pool_update.attach %s to %s" (Db.Pool_update.get_name_label ~__context ~self) mount_point;
  if (try Sys.is_directory mount_point with _ -> false) then detach ~__context ~self ~host;
  let output, _ = Forkhelpers.execute_command_get_output "/bin/mkdir" ["-p"; mount_point] in
  debug "pool_update.attach Mountpoint created (output=%s)" output;
  let vdi = Db.Pool_update.get_vdi ~__context ~self in
  let url = if vdi_visible ~__context ~self ~host then begin
      let device = Helpers.call_api_functions ~__context
          (fun rpc session_id ->
             let dom0 = Helpers.get_domain_zero ~__context in
             let vbd = Client.VBD.create ~rpc ~session_id ~vM:dom0 ~empty:false ~vDI:vdi
                 ~userdevice:"autodetect" ~bootable:false ~mode:`RO ~_type:`Disk ~unpluggable:true
                 ~qos_algorithm_type:"" ~qos_algorithm_params:[]
                 ~other_config:[] in
             Client.VBD.plug ~rpc ~session_id ~self:vbd;
             "/dev/" ^ (Client.VBD.get_device ~rpc ~session_id ~self:vbd)) in
      with_api_errors (mount device) mount_point;
      debug "pool_update.attach Mounted %s" mount_point;
      "file://" ^ mount_point
    end
    else begin
      let ip = try Pool_role.get_master_address () with _ -> "127.0.0.1" in
      "http://" ^ ip ^ mount_point
    end in
  create_yum_config ~__context ~self ~url:url ~location:mount_point_parent_dir;
  url

exception Missing_update_key of string
exception Bad_update_info
exception Invalid_update_uuid of string

let check_unsigned_update_fist uuid =
  match Xapi_fist.allowed_unsigned_updates () with
  | None -> false
  | Some fist ->
    debug "Pool update uuid: %s" uuid;
    let fist_uuids = String.split_f String.isspace fist in
    debug "FIST allowed_unsigned_updates: %s" fist;
    List.mem uuid fist_uuids

let guidance_from_string = function
  | "restartHVM"  -> `restartHVM
  | "restartPV"   -> `restartPV
  | "restartHost" -> `restartHost
  | "restartXAPI" -> `restartXAPI
  | _ -> raise Bad_update_info

let extract_update_info ~__context ~self ~verify ~destroy =
  let host = Helpers.get_localhost ~__context in
  finally
    (fun () ->
       let update_path = attach ~__context ~self ~host in
       let update_path =
         if String.startswith "file://" update_path then
           String.sub_to_end update_path (String.length "file://")
         else
           update_path
       in
       let xml = Xml.parse_file (String.concat "/" [update_path ; "update.xml"]) in
       match xml with
       | Xml.Element ("update", attr, children) ->
         let key = List.assoc "key" attr in
         let uuid = List.assoc "uuid" attr in
         let name_label = List.assoc "name-label" attr in
         let installation_size =
           try
             Int64.of_string (List.assoc "installation-size" attr)
           with
           | _ -> 0L
         in
         let guidance_mandatory =
           try
             bool_of_string (List.assoc "guidance-mandatory" attr)
           with
           | _ -> true
         in
         let guidance =
           try
             List.assoc "after-apply-guidance" attr
           with
           | _ -> ""
         in
         let guidance =
           match guidance_mandatory, guidance with
           | false, _ -> []
           | true, "" -> []
           | true, _ -> List.map guidance_from_string (String.split ' ' guidance)
         in
         let is_name_description_node = function
           | Xml.Element ("name-description", _, _) -> true
           | _ -> false
         in
         let name_description = match List.find is_name_description_node children with
           | Xml.Element("name-description", _, [ Xml.PCData s ]) -> s
           | _ -> failwith "Malformed or missing <name-description>"
         in
         let update_info = {
           uuid = uuid;
           name_label = name_label;
           name_description = name_description;
           key = key;
           installation_size = installation_size;
           after_apply_guidance = guidance;
         } in
         ignore(verify update_info);
         update_info
       | _ -> failwith "Malformed or missing <update>"
    )
    (fun () ->
       finally
         (fun () -> detach ~__context ~self ~host)
         (fun () -> if destroy then Db.Pool_update.destroy ~__context ~self)
    )

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
  let r = Ref.make () in
  let _ = Db.Pool_update.create ~__context
      ~ref:r
      ~uuid:(Uuid.string_of_uuid (Uuid.make_uuid ()))
      ~name_label:"deadbeef"
      ~name_description:"deadbeef"
      ~installation_size:0L
      ~key:""
      ~after_apply_guidance:[]
      ~vdi:vdi
  in
  let verify update_info =
    let update_xml_path = String.concat "/" [Xapi_globs.host_update_dir; update_info.uuid; "vdi"; "update.xml"] in
    let repomd_xml_path = String.concat "/" [Xapi_globs.host_update_dir; update_info.uuid; "vdi"; "repodata"; "repomd.xml"] in
    begin
      match check_unsigned_update_fist update_info.uuid with
      | false ->
        List.iter (fun filename ->
            let signature = filename ^ ".asc" in
            Gpg.with_verified_signature filename signature (fun fingerprint fd ->
                match fingerprint with
                | Some f ->
                  let enc = Base64.encode f in
                  let acceptable_keys =
                    match Xapi_fist.allow_test_updates () with
                    | false -> [ !Xapi_globs.trusted_update_key ]
                    | true -> [ !Xapi_globs.trusted_update_key; Xapi_globs.test_update_key ]
                  in
                  if List.mem enc acceptable_keys then
                    debug "Fingerprint verified."
                  else
                    debug "Got fingerprint: %s" f;
                  (*debug "Encoded: %s" (Base64.encode f); -- don't advertise the fact that we've got an encoded string in here! *)
                  raise Gpg.InvalidSignature
                | _ ->
                  debug "No fingerprint!";
                  raise Gpg.InvalidSignature
              )
          ) [update_xml_path; repomd_xml_path];
        debug "Verify signature OK for pool update uuid: %s by key: %s" update_info.uuid update_info.key
      | true ->
        debug "Verify signature bypass for pool update uuid: %s" update_info.uuid
    end
  in
  let update_info = extract_update_info ~__context ~self:r ~verify ~destroy:true in
  ignore(assert_space_available update_info.installation_size);
  let r = Ref.make () in
  Db.Pool_update.create ~__context
    ~ref:r
    ~uuid:update_info.uuid
    ~name_label:update_info.name_label
    ~name_description:update_info.name_description
    ~installation_size:update_info.installation_size
    ~key:update_info.key
    ~after_apply_guidance:update_info.after_apply_guidance
    ~vdi:vdi;
  r

let pool_apply ~__context ~self =
  let pool_update_name = Db.Pool_update.get_name_label ~__context ~self in
  debug "pool_update.pool_apply %s" pool_update_name;

  let applied_hosts = Db.Pool_update.get_hosts ~__context ~self in
  let candidate_hosts = List.set_difference (Db.Host.get_all ~__context) applied_hosts in

  List.iter (fun host ->
      ignore(Helpers.call_api_functions ~__context
               (fun rpc session_id -> Client.Pool_update.apply rpc session_id self host))
    ) candidate_hosts

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
  Db.Pool_update.destroy ~__context ~self
