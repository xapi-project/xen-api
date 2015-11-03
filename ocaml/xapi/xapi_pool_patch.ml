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
 
open Pervasiveext
open Xstringext
open Http
open Forkhelpers
open Xml
open Helpers
open Listext

module D = Debug.Make(struct let name="xapi" end)
open D
(** Patches contain their own metadata in XML format. When the signature has been verified
    the patch is executed with argument "info" and it emits XML like the following:

      <info  uuid="foo-bar-baz"
             version="1.0"
             name-label="My First Patch(TM)"
             name-description="This is a simple executable patch file used for testing"
             after-apply-guidance="restartHVM restartPV restartHost"
      />
*)
type patch_info = { uuid: string; name_label: string; 
                    name_description: string; version: string; 
                    after_apply_guidance: API.after_apply_guidance list }

exception Missing_patch_key of string
exception Bad_patch_info
exception Invalid_patch_uuid of string

let rm = "/bin/rm"

(* Host patches are directories are placed here: *)
let patch_dir = "/var/patch"

let check_unsigned_patch_fist path =
	match Xapi_fist.allowed_unsigned_patches () with
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
		debug "FIST allowed_unsigned_patches: %s" fist;
		List.mem sha1 fist_sha1s

let extract_patch path =
  let run_path = path ^ ".run" in
  try
    Unixext.with_file run_path [ Unix.O_WRONLY; Unix.O_CREAT ] 0o755
      (fun fd ->
	Gpg.with_signed_cleartext path
	  (fun fingerprint fd' -> 
	    (match fingerprint with 
	      | Some f ->
		  let enc = Base64.encode f in
		  let acceptable_keys = 
			  if Xapi_fist.allow_test_patches () then
				  [ !Xapi_globs.trusted_patch_key; Xapi_globs.test_patch_key ] else [ !Xapi_globs.trusted_patch_key ]
		  in
		  if not (List.mem enc acceptable_keys)
		  then 
		    (
                      debug "Got fingerprint: %s" f;
		      (*debug "Encoded: %s" (Base64.encode f); -- don't advertise the fact that we've got an encoded string in here! *)
		      raise Gpg.InvalidSignature
		    )
                  else
                    debug "Fingerprint verified."
	      | None ->
		  debug "No fingerprint!";
		  raise Gpg.InvalidSignature);
            let (_: int64) = Unixext.copy_file fd' fd in
			()
          )
      );
    run_path
  with e ->
    if check_unsigned_patch_fist path then begin
      debug "Patch not signed, but still letting it through";
      Unixext.with_file run_path [ Unix.O_WRONLY; Unix.O_CREAT ] 0o755
        (fun fd ->
          Unixext.with_file path [ Unix.O_RDONLY ] 0
            (fun fd' ->
              let (_: int64) = Unixext.copy_file fd' fd in
              run_path
            )
        )
    end else begin
      Unixext.unlink_safe run_path;
      raise e
    end



  (* We execute the patch with arguments (name-label|name-description|version|apply) to
     query its metadata and apply it *)
  let execute_patch path args =
    if not (Sys.file_exists path)
    then raise (Api_errors.Server_error (Api_errors.cannot_find_patch, []));

    debug "Stripping header on patch %s then running with args '%s'" path (String.concat " " args);
    let run_path = extract_patch path in
    finally
      (fun () ->
        with_logfile_fd "patch"
          (fun log_fd ->
            let pid = safe_close_and_exec None (Some log_fd) (Some log_fd) [] run_path args in
            waitpid_fail_if_bad_exit pid)
      )
      (fun () -> Unixext.unlink_safe run_path)

let guidance_from_string = function
  | "restartHVM"  -> `restartHVM
  | "restartPV"   -> `restartPV
  | "restartHost" -> `restartHost
  | "restartXAPI" -> `restartXAPI
  | _ -> raise Bad_patch_info

let precheck_patch_uuid uuid =
  let uuid = String.lowercase uuid in
  if not (Uuid.is_uuid uuid)
    then raise (Invalid_patch_uuid uuid);
  uuid

let patch_info_of_xml = function
  | Element("info", attr, _) ->
      let find x = 
	    if List.mem_assoc x attr 
	    then List.assoc x attr 
	    else raise (Missing_patch_key x) in
      let label = find "name-label" 
      and descr = find "name-description" 
      and version = find "version" 
      and uuid = precheck_patch_uuid (find "uuid")
      and guidance = find "after-apply-guidance"
      in
      let guidance =
        if guidance <> "" then
          let guidance = String.split ' ' guidance in
            List.map guidance_from_string guidance
        else
          []
      in
      { uuid = uuid; name_label = label; name_description = descr; 
        version = version; after_apply_guidance = guidance }
  | _ -> raise Bad_patch_info

let patch_info_of_string s = 
  let xml = Xml.parse_string s in
    debug "xml: %s" (Xml.to_string xml);
    patch_info_of_xml xml

let get_patch_info path =
  match execute_patch path [ "info" ] with
  | Success(output, _) ->
      begin
        try
          debug "executing patch successful; parsing XML" ;
          patch_info_of_string output
        with e ->
          raise (Api_errors.Server_error(Api_errors.invalid_patch, [Printexc.to_string e]))
      end
  | Failure(log, exn) ->
      debug "error from patch application: %s" log;
      begin
        match exn with
          | Subprocess_failed 2 ->
              debug "probably bad line endings...";
              raise (Api_errors.Server_error(Api_errors.invalid_patch_with_log, ["Bad line endings?"]))
          | _ -> raise exn      
      end

let read_in_and_check_patch length s path =
  try    
    debug "Will stream patch to file: %s" path;
    
    (* Stream the contents to path *)
    begin
      match length with
        | None  -> 
            Unixext.with_file path [ Unix.O_WRONLY; Unix.O_CREAT ] 0o440
	      (fun fd -> let (_: int64) = Unixext.copy_file s fd in ())
        | Some i ->
            Unixext.with_file path [ Unix.O_WRONLY; Unix.O_CREAT ] 0o440
	      (fun fd -> let (_: int64) = Unixext.copy_file ~limit:i s fd in ())
    end;

    debug "Streaming complete; executing gpg";
    
    let run_path = extract_patch path in
    Unixext.unlink_safe run_path
  with
    | Unix.Unix_error (errno, _, _) when errno = Unix.ENOSPC ->
       warn "Not enough space on filesystem to upload patch.";
       raise (Api_errors.Server_error (Api_errors.out_of_space, [patch_dir]))
    | exn ->
    debug "Caught exception while checking signature: %s" (ExnHelper.string_of_exn exn);
    Unixext.unlink_safe path;
    raise (Api_errors.Server_error(Api_errors.invalid_patch, []))

let create_patch_record ~__context ?path patch_info =
  let r = Ref.make () in
  let path, size = 
    match path with
      | None -> "", Int64.zero
      | Some path ->
          let stat = Unix.stat path in
            path, Int64.of_int stat.Unix.st_size
 in
  Db.Pool_patch.create ~__context ~ref:r 
    ~uuid:patch_info.uuid
	~name_label:patch_info.name_label 
    ~name_description:patch_info.name_description
	~version:patch_info.version
    ~filename:path 
    ~size
    ~pool_applied:false
    ~after_apply_guidance:patch_info.after_apply_guidance
    ~other_config:[];
  r

exception CannotUploadPatchToSlave

(* Experiments showed that we need about thrice the amount of free
   space on the filesystem as the size of the patch, which is where
   the multiplier comes from. *)
let assert_space_available ?(multiplier=3L) patch_size =
	let open Unixext in
	ignore (Unixext.mkdir_safe patch_dir 0o755);
	let stat = statvfs patch_dir in
	let free_bytes =
		(* block size times free blocks *)
		Int64.mul stat.f_frsize stat.f_bfree in
	let really_required = Int64.mul multiplier patch_size in
	if really_required > free_bytes
	then
		begin
			warn "Not enough space on filesystem to upload patch. Required %Ld, \
			but only %Ld available" really_required free_bytes;
			raise (Api_errors.Server_error (Api_errors.out_of_space, [patch_dir]))
		end

let pool_patch_upload_handler (req: Request.t) s _ =
  debug "Patch Upload Handler - Entered...";

  if not (Pool_role.is_master ())
  then raise CannotUploadPatchToSlave;
  
  Xapi_http.with_context "Uploading host patch" req s
    (fun __context ->
      if on_oem ~__context
      then raise (Api_errors.Server_error (Api_errors.not_allowed_on_oem_edition, ["patch-upload"]));
      
      debug "Patch Upload Handler - Authenticated...";

      let _ = Unixext.mkdir_safe patch_dir 0o755 in
      let new_path = patch_dir ^ "/" ^ (Uuid.to_string (Uuid.make_uuid ())) in
      let task_id = Context.get_task_id __context in
      begin
       
        debug "Patch Upload Handler - Sending headers...";

        Http_svr.headers s (Http.http_200_ok ());

        (match req.Request.content_length with
         | None -> ()
         | Some size -> assert_space_available size);

        read_in_and_check_patch req.Request.content_length s new_path;
	
        try
          let r = create_patch_record ~__context ~path:new_path (get_patch_info new_path) in
          Db.Task.set_result ~__context ~self:task_id ~value:(Ref.string_of r)   
        with Db_exn.Uniqueness_constraint_violation (_, _, uuid) ->
          (* patch already uploaded.  if the patch file has been cleaned, then put this one in its place. 
             otherwise, error *)
          debug "duplicate patch with uuid %s found." uuid;
          let patch_ref = Db.Pool_patch.get_by_uuid ~__context ~uuid in
          let old_path = Db.Pool_patch.get_filename ~__context ~self:patch_ref in
          debug "checking for file %s.  If it doesn't exist new patch will replace it." old_path;
          if Sys.file_exists old_path
          then
            begin
              Unixext.unlink_safe new_path;
              raise (Api_errors.Server_error(Api_errors.patch_already_exists, [uuid])) 
            end
          else
            begin
              let stat = Unix.stat new_path in
              let size = Int64.of_int stat.Unix.st_size in
                Db.Pool_patch.set_filename ~__context ~self:patch_ref ~value:new_path;
                Db.Pool_patch.set_size ~__context ~self:patch_ref ~value:size;
                Db.Task.set_result ~__context ~self:task_id ~value:(Ref.string_of patch_ref)   
            end
      end            
    )

let bin_sync = "/bin/sync"

let sync () =
  let output =
    with_logfile_fd "sync"
      (fun log_fd ->
         let pid = safe_close_and_exec None (Some log_fd) (Some log_fd) [] bin_sync [] in
         waitpid_fail_if_bad_exit pid) 
  in 
    match output with
      | Failure(log, exn) ->
          debug "error from sync application: %s" log;
          raise exn    
      | Success(output, _) -> ()

let patch_header_length = 8

let pool_patch_download_handler (req: Request.t) s _ =
  Xapi_http.with_context "Downloading pool patch" req s
    (fun __context ->     
      if not(List.mem_assoc "uuid" req.Request.query) then begin
        Http_svr.headers s (Http.http_400_badrequest ());
        error "HTTP request for pool patch lacked 'uuid' parameter"
      end else begin
        let uuid = List.assoc "uuid" req.Request.query in
        (* ensure its a valid uuid *)
        let r = Db.Pool_patch.get_by_uuid ~__context ~uuid in
        let path = Db.Pool_patch.get_filename ~__context ~self:r in
  
        if not (Sys.file_exists path)
        then raise (Api_errors.Server_error (Api_errors.cannot_find_patch, []));
  
        Http_svr.response_file s path;
      end;
      req.Request.close <- true
    )
      
let get_patch_to_local ~__context ~self =
  if not (Pool_role.is_master ()) then
    begin
      let length = Db.Pool_patch.get_size ~__context ~self in
      assert_space_available length;
      let path = Db.Pool_patch.get_filename ~__context ~self in
      let pool_secret = !Xapi_globs.pool_secret in
      let uuid = Db.Pool_patch.get_uuid ~__context ~self in
      Server_helpers.exec_with_new_task
	~task_in_database:true ~subtask_of:(Context.get_task_id __context)
	~session_id:(Context.get_session_id __context)
	(Printf.sprintf "Get patch %s from master" uuid)

	(fun __context ->
	 let task = Context.get_task_id __context in
	 let uri = Printf.sprintf "%s?pool_secret=%s&uuid=%s&task_id=%s"
		     Constants.pool_patch_download_uri
		     pool_secret uuid (Ref.string_of task) in
	 let request = Xapi_http.http_request ~version:"1.1" Http.Get uri in
	 let master_address = Pool_role.get_master_address () in
	 let open Xmlrpc_client in
	 let transport = SSL(SSL.make ~use_stunnel_cache:true
				      ~task_id:(Ref.string_of task) (),
			     master_address, !Xapi_globs.https_port) in

	 try
	   with_transport transport
			  (with_http request
				     (fun (response, fd) ->
				      let _ = Unixext.mkdir_safe patch_dir 0o755 in
				      read_in_and_check_patch (Some length) fd path))

	 with _ ->
	   begin
	     let error = Db.Task.get_error_info ~__context ~self:task in
	     if List.length error > 0
	     then
	       begin
		 debug "Error %s fetching patch from master." (List.hd error);
		 raise (Api_errors.Server_error (List.hd error, List.tl error))
	       end
	     else raise (Api_errors.Server_error
			   (Api_errors.cannot_fetch_patch, [uuid]))
	   end)
    end

open Db_filter
open Db_filter_types

let patch_apply_in_progress ~__context ~patch ~host =
	let message = Printf.sprintf "Applying the same patch on %s is in progress. If you believe this is not true or the patching process is hung/frozen, try to restart the toolstack or host." (Db.Host.get_name_label ~__context ~self:host) in
	raise (Api_errors.Server_error (Api_errors.other_operation_in_progress, ["Pool_patch"; Ref.string_of patch; message]))

(* The [get_patch_applied_to] gives the patching status of a pool patch on the given host. It
   returns [None] if the patch is not on the host, i.e. no corresponding host_patch;
   returns [Some (ref, true)] if it's on the host and fully applied (as host_patch [ref]);
   returns [Some (ref, false)] if it's on the host but isn't applied yet or the application is in progress. *)
let get_patch_applied_to ~__context ~patch ~host =
	let expr =
		And (Eq (Field "pool_patch", Literal (Ref.string_of patch)),
		     Eq (Field "host",  Literal (Ref.string_of host)))
	in
	let result = Db.Host_patch.get_records_where ~__context ~expr in
	match result with
	| [] -> None
	| (rf, rc) :: _ -> Some (rf, rc.API.host_patch_applied)

let patch_applied_dir = "/var/patch/applied"

let write_patch_applied ~__context ~self = 
  (* This will write a small file containing xml to /var/patch/applied/ detailing what patches have been applied*)
  (* This allows the agent to remember what patches have been applied across pool-ejects *)
  let path = Db.Pool_patch.get_filename ~__context ~self in
  let _ = Unixext.mkdir_safe patch_applied_dir 0o755 in
    match execute_patch path [ "info" ] with
      | Success(output, _) ->
          begin
            let uuid = Db.Pool_patch.get_uuid ~__context ~self in
            let path = patch_applied_dir ^ "/" ^ uuid in
              Unixext.with_file path [ Unix.O_WRONLY; Unix.O_CREAT ] 0o440
                (fun fd -> let (_: int) = Unix.write fd output 0 (String.length output) in ())
          end
      | Failure(log, exn) ->
          debug "error from patch application: %s" log;
          raise exn      


let write_patch_applied_db ~__context ?date ?(applied=true) ~self ~host () =
	let date = Date.of_float (match date with
	| Some d -> d
	| None -> Unix.gettimeofday ())
	in
	match get_patch_applied_to ~__context ~patch:self ~host with
	| Some(r, is_applied) ->
		 if not (is_applied = applied) then begin
			 Db.Host_patch.set_timestamp_applied ~__context ~self:r ~value:date;
			 Db.Host_patch.set_applied ~__context ~self:r ~value:applied
		 end
	| None ->
		let uuid = Uuid.make_uuid () in
		let r = Ref.make () in
		Db.Host_patch.create ~__context
			~ref:r
			~uuid:(Uuid.to_string uuid)
			~host
			~pool_patch:self
			~timestamp_applied:date
			~name_label:""
			~name_description:""
			~version:""
			~filename:""
			~applied
			~size:Int64.zero
			~other_config:[]

let erase_patch_applied_db ~__context ~self ~host () =
	match get_patch_applied_to ~__context ~patch:self ~host with
	| Some (r, _) ->
		 debug "Removing Host_patch record for patch %s" (Ref.string_of self);
		 Db.Host_patch.destroy ~__context ~self:r
	| None -> ()

let update_db ~__context =
  (* We need to check the patch_applied_dir for applied patches and check they are present in the db *)
  (* Used from dbsync_slave - DO NOT THROW ANY EXCEPTIONS *)
  try
    (* First look in the patch applied dir for the definitive list of locally-applied patches *)
    let local_patch_details = 
      (* Full paths of the /var/patch/applied files *)
      let stampfiles = List.map (Filename.concat patch_applied_dir) (try Array.to_list (Sys.readdir patch_applied_dir) with _ -> []) in
      let parse x = 
	try [ patch_info_of_string (Unixext.string_of_file x), (Unix.stat x).Unix.st_mtime ]
	with e -> warn "Error parsing patch stampfile %s: %s" x (ExnHelper.string_of_exn e); [] in
      List.concat (List.map parse stampfiles) in

    (* Make sure all the patches in the filesystem have global Pool_patch records *)
    let pool_patches_in_fs = List.map 
      (fun (details , _)-> 
	 try Db.Pool_patch.get_by_uuid ~__context ~uuid:details.uuid
	 with _ ->
	   debug "Patch uuid %s does not exist in Pool_patch table; creating" details.uuid;
	   create_patch_record ~__context details) local_patch_details in
    (* Construct a table of pool_patch to mtime, necessary if we create Host_patch records *)
    let pool_patch_to_mtime = List.combine pool_patches_in_fs (List.map snd local_patch_details) in

    (* Find this Host's Pool_patch records in the database *)
    let host_patches_in_db = Db.Host.get_patches ~__context ~self:(Helpers.get_localhost ~__context) in
    let pool_patches_in_db = List.map (fun hp -> Db.Host_patch.get_pool_patch ~__context ~self:hp) host_patches_in_db in
    (* We compare the referenced 'pool_patches' and then use this table to get back the localhost 'host_patch': *)
    let pool_patch_to_host_patch = List.combine pool_patches_in_db host_patches_in_db in

    (* Now perform a two-way sync between pool_patches_in_fs and pool_patches_in_db *)
    let new_pool_patches = List.set_difference pool_patches_in_fs pool_patches_in_db in
    let old_pool_patches = List.set_difference pool_patches_in_db pool_patches_in_fs in

    List.iter
      (fun pp ->
	 let msg = Printf.sprintf "Adding new Host_patch record for patch %s" (Ref.string_of pp) in
	 Helpers.log_exn_continue msg 
	   (fun () ->
	      debug "%s" msg;
	      let date = List.assoc pp pool_patch_to_mtime in
	      let host = Helpers.get_localhost ~__context in
	      write_patch_applied_db ~__context ~date ~self:pp ~host ()) ()) new_pool_patches;
    List.iter
      (fun pp ->
	 let msg = Printf.sprintf "Removing Host_patch record for patch %s" (Ref.string_of pp) in
	 Helpers.log_exn_continue msg
	   (fun () ->
	      debug "%s" msg;
	      Db.Host_patch.destroy ~__context ~self:(List.assoc pp pool_patch_to_host_patch)) ()) old_pool_patches
  with 
    | End_of_file ->
        ()
    | e ->
        debug "Error updating patch status. %s" (ExnHelper.string_of_exn e)

exception Bad_precheck_xml of string

let parse_patch_precheck_xml patch xml =
  let rec findElement name = function
    | Element (tagName, _, (PCData head)::_)::tail -> if tagName = name then head else findElement name tail
    | _::tail -> findElement name tail
    | [] -> raise (Bad_precheck_xml "Could not find element %s")
  in
  match xml with
  | Element ("error", [("errorcode", "PATCH_PRECHECK_FAILED_UNKNOWN_ERROR")], [Element("info", _, [PCData info])]) ->
      (* <error errorcode="PATCH_PRECHECK_FAILED_UNKNOWN_ERROR">
        <info>Any message in text - for errors that don't fit into another category</info>
      </error> *)
      raise (Api_errors.Server_error (Api_errors.patch_precheck_failed_unknown_error, [Ref.string_of patch; info]))
  | Element("error" , [("errorcode","PATCH_PRECHECK_FAILED_ISO_MOUNTED")], [Element ("info",_, [PCData info])]) ->
      raise (Api_errors.Server_error (Api_errors.patch_precheck_tools_iso_mounted, [Ref.string_of patch; info]))
  | Element ("error", [("errorcode", "PATCH_PRECHECK_FAILED_PREREQUISITE_MISSING")], children) ->
      (* <error errorcode="PATCH_PRECHECK_FAILED_PREREQUISITE_MISSING">
        <prerequisite uuid="ABCD1234-FEED-DEAD-BEEF-000000000000" />
        <prerequisite uuid="ABCD1234-FEED-DEAD-BEEF-000000000001" />
      </error> *)
      let rec collectUuids = function
        | (Element("prerequisite", [("uuid", uuid)], _))::tail -> uuid::(collectUuids tail)
        | [] -> []
        | _ -> raise (Bad_precheck_xml "Malformed prerequisite list")
      in
        let uuids = collectUuids children in
        raise (Api_errors.Server_error (Api_errors.patch_precheck_failed_prerequisite_missing, [Ref.string_of patch; String.concat ";" uuids]))
  | Element ("error", [("errorcode", "PATCH_PRECHECK_FAILED_WRONG_SERVER_VERSION")], children) ->
      (* <error errorcode="PATCH_PRECHECK_FAILED_WRONG_SERVER_VERSION">
        <found>4.0.91</found>
        <required>4.0.95 or newer</required>
      </error> *)
        let found = findElement "found" children in
        let required = findElement "required" children in
        raise (Api_errors.Server_error (Api_errors.patch_precheck_failed_wrong_server_version, [Ref.string_of patch; found; required]))
  | Element ("error", [("errorcode", "PATCH_PRECHECK_FAILED_WRONG_SERVER_BUILD")], children) ->
      (* Exactly like the previous one but SERVER_BUILD instead of SERVER_VERSION *)
      (* <error errorcode="PATCH_PRECHECK_FAILED_WRONG_SERVER_BUILD">
        <found>50845c</found>
        <required>^58332[pc]$</required>
      </error> *)
        let found = findElement "found" children in
        let required = findElement "required" children in
        raise (Api_errors.Server_error (Api_errors.patch_precheck_failed_wrong_server_build, [Ref.string_of patch; found; required]))
  | Element ("error", [("errorcode", "PATCH_PRECHECK_FAILED_VM_RUNNING")], _) ->
      (* <error errorcode="PATCH_PRECHECK_FAILED_VM_RUNNING" /> *)
      raise (Api_errors.Server_error (Api_errors.patch_precheck_failed_vm_running, [Ref.string_of patch]))
  | Element ("error", [("errorcode", "PATCH_PRECHECK_FAILED_OUT_OF_SPACE")], children) ->
      (* <error errorcode="PATCH_PRECHECK_FAILED_OUT_OF_SPACE">
       *   <found>165396480</found>
       *   <required>1073741824000</required>
       * </error>
       *)
      let found = findElement "found" children in
      let required = findElement "required" children in
      raise (Api_errors.Server_error (Api_errors.patch_precheck_failed_out_of_space, [Ref.string_of patch; found; required]))
  | _ ->
      raise (Bad_precheck_xml "Unknown error code or malformed xml")
  
(* calls the parse function, which throws the correct error based on the XML the patch precheck put on stdout *)
let throw_patch_precheck_error patch s =
  try
    let xml = Xml.parse_string s in
      debug "precheck xml: %s" (Xml.to_string xml);
      parse_patch_precheck_xml patch xml
  with 
  | Xml.Error error ->
    let msg = Printf.sprintf "error parsing patch precheck xml: %s" (Xml.error error) in
      debug "%s" msg;
      raise (Api_errors.Server_error (Api_errors.invalid_patch_with_log, [msg]))
  | Bad_precheck_xml error ->
      raise (Api_errors.Server_error (Api_errors.invalid_patch_with_log, [error]))

let run_precheck ~__context ~self ~host =
  let path = Db.Pool_patch.get_filename ~__context ~self in
    match execute_patch path [ "precheck" ] with
      | Success(output, _) -> output
      | Failure(xml, Subprocess_failed 1) ->
        (* if precheck returns 1, the patch should have written error xml to stdout *)
        debug "Prechecks on patch %s failed with return code 1. XML is %s" (Ref.string_of self) xml;
        throw_patch_precheck_error self xml
      | Failure(log, _) ->
        let msg = Printf.sprintf "Error running prechecks on patch %s: %s" (Ref.string_of self) log in
          debug "%s" msg;
          raise (Api_errors.Server_error(Api_errors.patch_precheck_failed_unknown_error, [Ref.string_of self; msg]))

(* precheck API call entrypoint *)
let precheck ~__context ~self ~host =
  (* check we're not on oem *)
  if on_oem ~__context
    then raise (Api_errors.Server_error (Api_errors.not_allowed_on_oem_edition, ["patch-precheck"]));

  (* get the patch from the master (no-op if we're the master) *)
  get_patch_to_local ~__context ~self;

  finally 
	  (fun () -> run_precheck ~__context ~self ~host)
	  (fun () ->
		   (* This prevents leaking space on the slave if the patch is repeatedly uploaded, prechecked and then destroyed *)
		   if not (Pool_role.is_master ()) then begin		   
			 let path = Db.Pool_patch.get_filename ~__context ~self in
			 Unixext.unlink_safe path;		   
		   end
	  )

let apply ~__context ~self ~host = 
  (* 0th, check we're not on oem *)
  if on_oem ~__context
  then raise (Api_errors.Server_error (Api_errors.not_allowed_on_oem_edition, ["patch-apply"]));

  (* 1st, check patch isn't already applied *)
  let () = match get_patch_applied_to ~__context ~patch:self ~host with
  | Some (r, applied) ->
    if applied then raise (Api_errors.Server_error(Api_errors.patch_already_applied, [ Ref.string_of self ]))
    else patch_apply_in_progress ~__context ~patch:self ~host
  | None -> () in

  (* 2nd, get the patch from the master (no-op if we're the master) *)
  get_patch_to_local ~__context ~self;
  
  let path = Db.Pool_patch.get_filename ~__context ~self in
    (* 3rd, run prechecks *)
    let (_: string) = run_precheck ~__context ~self ~host in
 
    (* 4th, apply the patch *)
    begin
      write_patch_applied_db ~__context ~applied:false ~self ~host ();
      match execute_patch path [ "apply" ] with
        | Success(output, _) ->
	        debug "executing patch successful";
            write_patch_applied_db ~__context ~self ~host ();
            (* 5th, write out patch applied file to hd *)
            write_patch_applied ~__context ~self;
	    (* CA-27145: to handle rolled-up patches, rescan the patch applied directory *)
	    begin
	      try update_db ~__context
	      with e ->
		(* should never happen but just in case... *)
		error "Caught exception rescanning patch applied directory: %s" (ExnHelper.string_of_exn e)
	    end;
	        output;
		
        | Failure(log, exn) ->
		debug "error from patch application: %s" log;
           	erase_patch_applied_db ~__context ~self ~host ();
           	let error_string = "Backup files already present" in
           	if List.length (Xstringext.String.find_all error_string log) = 0 then
               		raise (Api_errors.Server_error(Api_errors.patch_apply_failed, [log]))
           	else begin
               		let xml = Xml.parse_string log in
               		match xml with
               		| Element ("error", [("errorcode", "PATCH_PRECHECK_FAILED_UNKNOWN_ERROR")], [Element("info", _, [PCData info])]) ->
                    		raise (Api_errors.Server_error(Api_errors.patch_apply_failed_backup_files_exist, [info]))
               		| _ ->
                   		raise (Bad_precheck_xml "Could not find element info")
           	end
    end

let pool_apply ~__context ~self =
  let hosts =
    List.filter 
      (fun x ->
         not (is_oem ~__context ~host:x)
         && match get_patch_applied_to ~__context ~patch:self ~host:x with
           | None -> true
           | Some (_, applied) -> not applied &&  patch_apply_in_progress ~__context ~patch:self ~host:x)
      (Db.Host.get_all ~__context) 
  in
  let (_: string list) = 
    List.map 
      (fun host ->
         Helpers.call_api_functions ~__context
           (fun rpc session_id -> Client.Client.Pool_patch.apply ~rpc ~session_id ~self ~host)
      )
      hosts 
  in
  let _ = Db.Pool_patch.set_pool_applied ~__context ~self ~value:true in
    ()
  
let clean ~__context ~self = 
  let path = Db.Pool_patch.get_filename ~__context ~self in
	Unixext.unlink_safe path

let clean_on_host ~__context ~self ~host = 
	debug "pool_patch.clean_on_host";
	clean ~__context ~self

let pool_clean ~__context ~self = 
	debug "pool_patch.pool_clean";
	let hosts = Db.Host.get_all ~__context in
	List.iter 
		(fun host ->
			Helpers.call_api_functions ~__context
				(fun rpc session_id -> Client.Client.Pool_patch.clean_on_host ~rpc ~session_id ~self ~host)
		)
		hosts; 
	Db.Pool_patch.set_filename ~__context ~self ~value:""

let destroy ~__context ~self = 
  let hosts = Db.Host.get_all ~__context in
  let applied = List.exists (fun host -> get_patch_applied_to ~__context ~patch:self ~host <> None ) hosts in

  if applied
  then raise (Api_errors.Server_error(Api_errors.patch_is_applied, []));

	List.iter 
		(fun host ->
			Helpers.call_api_functions ~__context
				(fun rpc session_id -> Client.Client.Pool_patch.clean_on_host ~rpc ~session_id ~self ~host)
		)
		hosts; 
	Db.Pool_patch.destroy ~__context ~self
