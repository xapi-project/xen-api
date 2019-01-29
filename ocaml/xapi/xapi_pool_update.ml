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
open Stdext.Threadext
open Unixext

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
  version: string;
  key: string;
  installation_size: int64;
  after_apply_guidance: API.after_apply_guidance list;
  enforce_homogeneity: bool; (* true = all hosts in a pool must have this update *)
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
    with
    | Forkhelpers.Spawn_internal_error(stderr, stdout, status) as e ->
      debug "Caught exception (%s) while unmounting %s: pausing before retrying"
        (ExnHelper.string_of_exn e) dest;
      begin match status with
        | Unix.WEXITED (32) -> finished := true (*not mounted*)
        | _ -> Thread.delay 5.
      end
    | e ->
      if not(retry) then raise e;
      debug "Caught exception (%s) while unmounting %s: pausing before retrying"
        (ExnHelper.string_of_exn e) dest;
      Thread.delay 5.
  done;
  if not(!finished) then raise Umount_timeout

let updates_to_attach_count_tbl : (string, int) Hashtbl.t = Hashtbl.create 10
let updates_to_attach_count_tbl_mutex = Mutex.create ()

let with_dec_refcount ~__context ~uuid ~vdi f =
  Mutex.execute updates_to_attach_count_tbl_mutex
    ( fun () ->
        let count = try Hashtbl.find updates_to_attach_count_tbl uuid with _ -> 0 in
        debug "pool_update.detach_helper '%s' count=%d" uuid count;
        if count <= 1 then begin
          f ~__context ~uuid ~vdi
        end;
        if count > 1 then
          Hashtbl.replace updates_to_attach_count_tbl uuid (count - 1)
        else if count = 1 then
          Hashtbl.remove updates_to_attach_count_tbl uuid
    )

let with_inc_refcount ~__context ~uuid ~vdi f =
  Mutex.execute updates_to_attach_count_tbl_mutex
    ( fun () ->
        let count = try Hashtbl.find updates_to_attach_count_tbl uuid with _ -> 0 in
        debug "pool_update.attach_helper refcount='%d'" (count);
        if count = 0 then begin
          f ~__context ~uuid ~vdi
        end;
        Hashtbl.replace updates_to_attach_count_tbl uuid (count + 1)
    )

let get_locally_attached ~__context ~uuid ~vdi =
    let mount_dir = Filename.concat !Xapi_globs.host_update_dir uuid in
    let mount_dir_opt = if Sys.file_exists mount_dir then Some mount_dir else None in
    let dom0 = Helpers.get_domain_zero ~__context in
    let vbds = List.filter (fun self -> Db.VBD.get_VM ~__context ~self = dom0) (Db.VDI.get_VBDs ~__context ~self:vdi) in 
    (mount_dir_opt, vbds)

let detach_helper ~__context ~uuid ~vdi =
  with_dec_refcount ~__context ~uuid ~vdi
    (fun ~__context ~uuid ~vdi ->
       let mount_point_parent_dir = String.concat "/" [!Xapi_globs.host_update_dir; uuid] in
       let mount_point = Filename.concat mount_point_parent_dir "vdi" in
       debug "pool_update.detach_helper %s from %s" uuid mount_point;
       if try Sys.is_directory mount_point with _ -> false then begin
         Helpers.log_exn_continue ("detach_helper: unmounting " ^ mount_point)
           (fun () -> umount mount_point) ()
       end;
       Helpers.call_api_functions ~__context (fun rpc session_id ->
           let dom0 = Helpers.get_domain_zero ~__context in
           let vbds = Client.VDI.get_VBDs ~rpc ~session_id ~self:vdi in
           List.iter (fun self ->
               if Client.VBD.get_VM ~rpc ~session_id ~self = dom0 then begin
                 Client.VBD.unplug ~rpc ~session_id ~self;
                 Client.VBD.destroy ~rpc ~session_id ~self
               end) vbds);
       if try Sys.is_directory mount_point_parent_dir with _ -> false then begin
         Helpers.log_exn_continue ("pool_update.detach_helper: rm " ^ mount_point)
           (fun () ->
              let output, _ = Forkhelpers.execute_command_get_output "/bin/rm" ["-r"; mount_point] in
              debug "pool_update.detach_helper Mountpoint removed (output=%s)" output) ();
         Helpers.log_exn_continue ("pool_update.detach_helper: rmdir " ^ mount_point_parent_dir)
           (fun () ->
              let output, _ = Forkhelpers.execute_command_get_output "/bin/rmdir" ["--ignore-fail-on-non-empty"; mount_point_parent_dir] in
              debug "pool_update.detach_helper Mountpoint parent dir removed (output=%s)" output
           ) ()
       end;
    )

let detach ~__context ~self =
  let uuid = Db.Pool_update.get_uuid ~__context ~self in
  let vdi = Db.Pool_update.get_vdi ~__context ~self in
  detach_helper ~__context ~uuid ~vdi

let with_api_errors f x =
  let errinfo = "Please upload a valid package." in
  try f x
  with
  | Smint.Command_failed(ret, status, stdout_log, stderr_log)
  | Smint.Command_killed(ret, status, stdout_log, stderr_log) ->
    let msg = Printf.sprintf "Smint.Command_{failed,killed} ret = %d; status = %s; stdout = %s; stderr = %s"
        ret status stdout_log stderr_log in
    error "%s" msg;
    raise (Api_errors.Server_error (Api_errors.invalid_update, [errinfo]))
  | e ->
    let msg = ExnHelper.string_of_exn e in
    error "%s" msg;
    raise (Api_errors.Server_error (Api_errors.invalid_update, [errinfo]))

(* yum confif example
   [main]
   keepcache=0
   reposdir=/dev/null
   gpgcheck=$signed
   repo_gpgcheck=$signed
   installonlypkgs=

   [$label]
   name=$label
   baseurl=url
   ${signed:+gpgkey=file:///etc/pki/rpm-gpg/key}
*)
let create_yum_config ~__context ~self ~url =
  debug "pool_update.create_yum_config";
  let key = Db.Pool_update.get_key ~__context ~self in
  let signed = String.length key <> 0 in
  let signed_index = if signed then 1 else 0 in
  let name_label = Db.Pool_update.get_name_label ~__context ~self in
  String.concat "\n"
  [ "[main]"
  ; "keepcache=0"
  ; "reposdir=/dev/null"
  ; Printf.sprintf "gpgcheck=%d" signed_index
  ; Printf.sprintf "repo_gpgcheck=%d" signed_index
  ; "installonlypkgs="
  ; ""
  ; Printf.sprintf "[%s]" name_label
  ; Printf.sprintf "name=%s" name_label
  ; Printf.sprintf "baseurl=%s" url
  ; if signed then Printf.sprintf ("gpgkey=file:///etc/pki/rpm-gpg/%s") key else ""
  ; "" (* Newline at the end of the file *)
  ]

let attach_helper ~__context ~uuid ~vdi ~use_localhost_proxy =
  let host = Helpers.get_localhost ~__context in
  with_inc_refcount ~__context ~uuid ~vdi
    (fun ~__context ~uuid ~vdi ->
       let mount_point_parent_dir = String.concat "/" [!Xapi_globs.host_update_dir; uuid] in
       let mount_point = Filename.concat mount_point_parent_dir "vdi" in
       debug "pool_update.attach_helper %s to %s" uuid mount_point;
       let output, _ = Forkhelpers.execute_command_get_output "/bin/mkdir" ["-p"; mount_point] in
       debug "pool_update.attach_helper Mountpoint created (output=%s)" output;
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
       debug "pool_update.attach_helper Mounted %s" mount_point
    );
  let ip = if use_localhost_proxy then "127.0.0.1" else Db.Host.get_address ~__context ~self:host in 
  "http://" ^ ip ^ Constants.get_pool_update_download_uri ^ (Db.Host.get_uuid ~__context ~self:host) ^ "/" ^ uuid ^ "/vdi"

let attach ~__context ~self ~use_localhost_proxy =
  let uuid = Db.Pool_update.get_uuid ~__context ~self in
  let vdi = Db.Pool_update.get_vdi ~__context ~self in
  create_yum_config ~__context ~self ~url:(attach_helper ~__context ~uuid ~vdi ~use_localhost_proxy)

exception Bad_update_info
exception Invalid_update_uuid of string

let guidance_from_string = function
  | "restartHVM"  -> `restartHVM
  | "restartPV"   -> `restartPV
  | "restartHost" -> `restartHost
  | "restartXAPI" -> `restartXAPI
  | _ -> raise Bad_update_info

let parse_update_info xml =
  match xml with
  | Xml.Element ("update", attr, children) ->
    let key = try List.assoc "key" attr with _ -> "" in
    let uuid = try List.assoc "uuid" attr
      with _ -> raise (Api_errors.Server_error(Api_errors.invalid_update, ["missing <uuid> in update.xml"]))
    in
    let name_label = try List.assoc "name-label" attr
      with _ -> raise (Api_errors.Server_error(Api_errors.invalid_update, ["missing <name-label> in update.xml"]))
    in
    let version = try List.assoc "version" attr
      with _ -> raise (Api_errors.Server_error(Api_errors.invalid_update, ["missing <version> in update.xml"]))
    in
    let installation_size =
      try
        Int64.of_string (List.assoc "installation-size" attr)
      with
      | _ -> 0L
    in
    let guidance =
      try
        match List.assoc "after-apply-guidance" attr with
        | "" -> []
        | s ->  List.map guidance_from_string (String.split ',' s)
      with
      | _ -> []
    in
    let enforce_homogeneity =
      Vm_platform.is_true ~key:"enforce-homogeneity" ~platformdata:attr ~default:false
    in
    let is_name_description_node = function
      | Xml.Element ("name-description", _, _) -> true
      | _ -> false
    in
    let name_description = match List.find is_name_description_node children with
      | Xml.Element("name-description", _, [ Xml.PCData s ]) -> s
      | _ -> raise (Api_errors.Server_error(Api_errors.invalid_update, ["missing <name-description> in update.xml"]))
    in
      { uuid
      ; name_label
      ; name_description
      ; version
      ; key = Filename.basename key
      ; installation_size
      ; after_apply_guidance = guidance
      ; enforce_homogeneity
      }
  | _ -> raise (Api_errors.Server_error(Api_errors.invalid_update, ["missing <update> in update.xml"]))

let extract_applied_update_info applied_uuid  =
  let applied_update = Printf.sprintf "%s/applied/%s" !Xapi_globs.host_update_dir applied_uuid in
  debug "pool_update.extract_applied_update_info, will parse '%s'" applied_update;
  let xml = Xml.parse_file applied_update in
  parse_update_info xml

let extract_update_info ~__context ~vdi ~verify =
  let vdi_uuid = Db.VDI.get_uuid ~__context ~self:vdi in
  finally
    (fun () ->
       let url = attach_helper ~__context ~uuid:vdi_uuid ~vdi ~use_localhost_proxy:true in
       let update_path = Printf.sprintf "%s/%s/vdi" !Xapi_globs.host_update_dir vdi_uuid in
       debug "pool_update.extract_update_info get url='%s', will parse_file in '%s'" url update_path;
       let xml = try
         Xml.parse_file (Filename.concat update_path "update.xml")
       with _ ->
         raise (Api_errors.Server_error (Api_errors.invalid_update, ["missing update document (update.xml) in the package."]))
       in
       let update_info = parse_update_info xml in
       ignore(verify update_info update_path); update_info
    )
    (fun () -> detach_helper ~__context ~uuid:vdi_uuid ~vdi)

let get_free_bytes path =
    let stat = statvfs path in
    (* block size times free blocks *)
    Int64.mul stat.f_frsize stat.f_bfree

let assert_space_available ?(multiplier=3L) ?(get_free_bytes=get_free_bytes) update_dir update_size =
  let free_bytes = get_free_bytes update_dir in
  let really_required = Int64.mul multiplier update_size in
  if really_required > free_bytes
  then
    begin
      error "Not enough space on filesystem to upload update. Required %Ld, \
             but only %Ld available" really_required free_bytes;
      raise (Api_errors.Server_error (Api_errors.out_of_space, [!Xapi_globs.host_update_dir]))
    end

exception Cannot_expose_yum_repo_on_slave

let verify update_info update_path =
  let update_xml_path = Filename.concat update_path "update.xml" in
  let repomd_xml_path = Filename.concat update_path "repodata/repomd.xml" in
  List.iter (fun filename ->
      let signature = filename ^ ".asc" in
      try
        Gpg.with_verified_signature filename signature (fun fingerprint fd ->
            match fingerprint with
            | Some f ->
              debug "Fingerprint '%s' verified." f
            | _ ->
              begin
                debug "No fingerprint!";
                raise (Api_errors.Server_error (Api_errors.invalid_update, ["Invalid signature"]))
              end
          )
      with exn ->
        debug "Caught exception while checking signature: %s" (ExnHelper.string_of_exn exn);
        raise (Api_errors.Server_error (Api_errors.invalid_update, ["Invalid signature"]))
    ) [update_xml_path; repomd_xml_path];
  debug "Verify signature OK for pool update uuid: %s by key: %s" update_info.uuid update_info.key

let patch_uuid_of_update_uuid uuid =
  let arr = Uuid.int_array_of_uuid (Uuid.uuid_of_string uuid) in
  let modify x = arr.(x) <- 0 in
  modify 4; modify 5; modify 6; modify 7;
  Uuid.uuid_of_int_array arr |> Uuid.string_of_uuid

let create_update_record ~__context ~update ~update_info ~vdi =
  let patch_ref = Ref.make () in
  ignore(Db.Pool_patch.create ~__context
           ~ref:patch_ref ~uuid:(patch_uuid_of_update_uuid update_info.uuid)
           ~name_label:update_info.name_label
           ~name_description:update_info.name_description
           ~version:update_info.version
           ~filename:""
           ~size:update_info.installation_size
           ~pool_applied:false
           ~after_apply_guidance:update_info.after_apply_guidance
           ~pool_update:update
           ~other_config:[]);
  Db.Pool_update.create ~__context
    ~ref:update
    ~uuid:update_info.uuid
    ~name_label:update_info.name_label
    ~name_description:update_info.name_description
    ~version:update_info.version
    ~installation_size:update_info.installation_size
    ~key:update_info.key
    ~after_apply_guidance:update_info.after_apply_guidance
    ~vdi:vdi
    ~other_config:[]
    ~enforce_homogeneity:update_info.enforce_homogeneity

let introduce ~__context ~vdi =
  if Db.VDI.get_type ~__context ~self:vdi = `cbt_metadata then begin
    error "pool_update.introduce: the given VDI has type cbt_metadata (at %s)" __LOC__;
    raise Api_errors.(Server_error(vdi_incompatible_type, [ Ref.string_of vdi; Record_util.vdi_type_to_string `cbt_metadata ]))
  end;

  ignore(Unixext.mkdir_safe !Xapi_globs.host_update_dir 0o755);
  (*If current disk free space is smaller than 1MB raise exception*)
  assert_space_available ~multiplier:1L !Xapi_globs.host_update_dir (Int64.mul 1024L 1024L);
  let update_info = extract_update_info ~__context ~vdi ~verify in
  try
    let update = Db.Pool_update.get_by_uuid ~__context ~uuid:update_info.uuid in
    let vdi_of_update = Db.Pool_update.get_vdi ~__context ~self:update in
    if not (Db.is_valid_ref __context vdi_of_update) then begin
        Db.Pool_update.set_vdi ~__context ~self:update ~value:vdi;
        update
    end 
    else if vdi <> vdi_of_update then 
        raise (Api_errors.Server_error(Api_errors.update_already_exists, [update_info.uuid]))
    else
        update
  with
  | Db_exn.Read_missing_uuid (_,_,_) -> 
    let update = Ref.make () in
    create_update_record ~__context ~update ~update_info ~vdi;
    update

let pool_apply ~__context ~self =
  let pool_update_name = Db.Pool_update.get_name_label ~__context ~self in
  debug "pool_update.pool_apply %s" pool_update_name;
  let unapplied_hosts = Db.Pool_update.get_hosts ~__context ~self |>
                        List.set_difference (Db.Host.get_all ~__context) in
  if List.length unapplied_hosts = 0
  then begin
    debug "pool_update.pool_apply, %s has already been applied on all hosts." pool_update_name;
    raise (Api_errors.Server_error(Api_errors.update_already_applied_in_pool, [Ref.string_of self]))
  end
  else
    let failed_hosts = unapplied_hosts |>
                       List.fold_left (fun acc host ->
                           try
                             ignore(Helpers.call_api_functions ~__context
                                      (fun rpc session_id -> Client.Pool_update.apply rpc session_id self host));
                             acc
                           with e ->
                             debug "Caught exception while pool_apply %s: %s" (Ref.string_of host) (ExnHelper.string_of_exn e);
                             host :: acc
                         ) [] in
    if List.length failed_hosts > 0 then raise (Api_errors.Server_error(Api_errors.update_pool_apply_failed, (List.map Ref.string_of failed_hosts)))


let pool_clean ~__context ~self =
  let pool_update_name = Db.Pool_update.get_name_label ~__context ~self in
  debug "pool_update.pool_clean %s" pool_update_name;
  detach ~__context ~self;
  let vdi = Db.Pool_update.get_vdi ~__context ~self in
  Helpers.call_api_functions ~__context
    (fun rpc session_id -> Client.VDI.destroy rpc session_id vdi);
  Db.Pool_update.set_vdi ~__context ~self ~value:Ref.null

let destroy ~__context ~self =
  let pool_update_name = Db.Pool_update.get_name_label ~__context ~self in
  debug "pool_update.destroy %s" pool_update_name;
  match Db.Pool_update.get_hosts ~__context ~self with
  | [] ->
    let patch = Xapi_pool_patch.pool_patch_of_update __context self in
    Db.Pool_update.destroy ~__context ~self;
    Db.Pool_patch.destroy ~__context ~self:patch;
    Db_gc_util.gc_updates_requiring_reboot ~__context
  | _ -> raise (Api_errors.Server_error(Api_errors.update_is_applied, []))

let detach_attached_updates __context =
  Db.Pool_update.get_all ~__context |>
  List.iter ( fun self ->
      let uuid = Db.Pool_update.get_uuid ~__context ~self in
      let vdi = Db.Pool_update.get_vdi ~__context ~self in
      match get_locally_attached ~__context ~uuid ~vdi with
      | None, [] -> ()
      | _ -> Helpers.log_exn_continue ("detach_attached_updates: update_uuid " ^ uuid) (fun () -> detach_helper ~__context ~uuid ~vdi) ()
    )

let resync_host ~__context ~host =
  let update_applied_dir = Filename.concat !Xapi_globs.host_update_dir "applied" in
  if Sys.file_exists update_applied_dir then begin
    debug "pool_update.resync_host scanning directory %s for applied updates" update_applied_dir;
    let updates_applied = try Array.to_list (Sys.readdir update_applied_dir) with _ -> [] in
    let update_uuids = List.filter (fun update -> Uuid.is_uuid update) updates_applied in
    let exists uuid =
      try Some (Db.Pool_update.get_by_uuid ~__context ~uuid)
      with _ -> None in
    (* Handle the updates rolluped and create records accordingly *)
    List.iter (fun update_uuid ->
        let update_info = extract_applied_update_info update_uuid in
        ( match exists update_uuid with
          | Some self ->
            (* re-interpret the enforce_homogeneity flag CP-258536 *)
            debug "pool_update.resync_host: update %s exists - updating it" update_uuid;
            Db.Pool_update.set_enforce_homogeneity ~__context ~self
              ~value:update_info.enforce_homogeneity
          | None ->
            let update = Ref.make () in
            debug "pool_update.resync_host: update %s not in database - creating it" update_uuid;
            create_update_record ~__context ~update:update ~update_info ~vdi:Ref.null
        )) update_uuids;
    let update_refs = List.map (fun update_uuid ->
        Db.Pool_update.get_by_uuid ~__context ~uuid:update_uuid) update_uuids in
    Db.Host.set_updates ~__context ~self:host ~value:update_refs;

    List.iter (fun update_ref ->
        let pool_patch_ref = Xapi_pool_patch.pool_patch_of_update ~__context update_ref in
        let uuid = Db.Pool_update.get_uuid ~__context ~self:update_ref in
        let mtime = (Unix.stat (Filename.concat update_applied_dir uuid)).Unix.st_mtime in
        Xapi_pool_patch.write_patch_applied_db ~__context ~date:mtime ~self:pool_patch_ref ~host ();
        (* The enforce_homogeneity flag is now stored in an update. In
         * Honolulu it was stored in pool patches. To avoid any confusion, we
         * delete it there. CA-260352 *)
        Db.Pool_patch.remove_from_other_config ~__context ~self:pool_patch_ref ~key:"enforce_homogeneity";
      ) update_refs;
    Create_misc.create_updates_requiring_reboot_info ~__context ~host;
    let host_info = Create_misc.read_localhost_info ~__context in
    Create_misc.create_software_version ~__context host_info
  end
  else Db.Host.set_updates ~__context ~self:host ~value:[];

  (* Clean up existing patches and updates if rolling upgrade has actually finished *)
  if not (Helpers.rolling_upgrade_in_progress ~__context) then begin
    (* Remove any pool_patch objects that don't have a corresponding pool_update object *)
    Db.Pool_patch.get_all ~__context
    |> List.filter (fun self -> Db.Pool_patch.get_pool_update ~__context ~self = Ref.null)
    |> List.iter (fun self ->
        (* Destroy connector before destroying Pool_patch *)
        Db.Pool_patch.get_host_patches ~__context ~self
        |> List.iter (fun self ->
            if Db.Host_patch.get_host ~__context ~self = host then
              Db.Host_patch.destroy ~__context ~self);
        Db.Pool_patch.destroy ~__context ~self);

    (* Clean updates that don't have a corresponding patch record *)
    Db.Pool_update.get_all ~__context
    |> List.filter (fun self -> Xapi_pool_patch.pool_patch_of_update ~__context self = Ref.null)
    |> List.iter (fun self -> destroy ~__context ~self);

    (*
     * If db indicates an update is not applied to any host but the corresponding patch is applied
     * on some host(s), that means the RPU has completed, the update record should be removed.
     * (The patch record will be removed along with the update record.)
    *)
    Db.Pool_update.get_all ~__context
    |> List.filter (fun self ->
         Db.Pool_update.get_hosts ~__context ~self = []
         && Xapi_pool_patch.pool_patch_of_update ~__context self
            |> fun self -> Db.Pool_patch.get_host_patches ~__context ~self
            |> function [] -> false | _ -> true)
    |> List.iter (fun self -> destroy ~__context ~self)
  end

let path_and_host_from_uri uri =
  (* remove any dodgy use of "." or ".." NB we don't prevent the use of symlinks *)
  let host_and_path = String.sub_to_end uri (String.length Constants.get_pool_update_download_uri) in
  match String.split ~limit:2 '/' host_and_path with
  | [host; untrusted_path] ->
    let resolved_path = untrusted_path
      |> Filename.concat !Xapi_globs.host_update_dir
      |> Uri.pct_decode
      |> Stdext.Unixext.resolve_dot_and_dotdot
    in
    (host,resolved_path)
  | _ -> failwith "Expecting both host and path in the uri"

let proxy_request req s host_uuid =
  Server_helpers.exec_with_new_task "pool_update_proxy_request" (fun __context ->
    let host =
      try Some (Db.Host.get_by_uuid ~__context ~uuid:host_uuid) with _ -> None
    in
    match host with
    |Some host ->
      let ip = Db.Host.get_address ~__context ~self:host in
      let transport = Xmlrpc_client.(SSL(SSL.make (), ip, 443)) in
      Xmlrpc_client.with_transport transport (fun fd ->
        Unixext.really_write_string fd (Http.Request.to_wire_string req);
        Unixext.proxy (Unix.dup s) (Unix.dup fd))
    |None ->
      debug "Caught exception while get Host by uuid %s" host_uuid;
      Http_svr.response_badrequest ~req s
  )

let pool_update_download_handler (req: Request.t) s _ =
  debug "pool_update.pool_update_download_handler URL %s" req.Request.uri;
  req.Request.close <- true;
  let localhost_uuid = Helpers.get_localhost_uuid () in
  let (host_uuid, filepath) = path_and_host_from_uri req.Request.uri in
  debug "pool_update.pool_update_download_handler %s" filepath;
  if host_uuid <> localhost_uuid then
    proxy_request req s host_uuid
  else begin
    if not(String.startswith !Xapi_globs.host_update_dir filepath) || not (Sys.file_exists filepath) then begin
      debug "Rejecting request for file: %s (outside of or not existed in directory %s)" filepath !Xapi_globs.host_update_dir;
      Http_svr.response_forbidden ~req s
    end else begin
      Fileserver.response_file s filepath
    end
  end
