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


open Client

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

let pool_patch_of_update ~__context update_ref =
  match Db.Pool_patch.get_refs_where ~__context
          ~expr:Db_filter_types.(Eq (Field "pool_update", Literal (Ref.string_of update_ref)))
  with
  | [patch] -> patch
  | patches ->
    error "Invalid state: Expected invariant - 1 pool_patch per pool_update. Found: [%s]"
      (String.concat ";" (List.map (fun patch -> Ref.string_of patch) patches));
    raise Api_errors.(Server_error (internal_error, ["Invalid state"]))


let pool_patch_upload_handler (req: Http.Request.t) s _ =
  debug "Patch Upload Handler - Entered...";

  Xapi_http.with_context "Uploading update" req s
    (fun __context ->
       Helpers.call_api_functions ~__context (fun rpc session_id ->
           (* Strip out the task info here, we'll use a new subtask. This
              is to avoid our task being prematurely marked as completed by
              the import_raw_vdi handler. *)
           let strip = List.filter (fun (k,v) -> k <> "task_id") in
           let add_sr query =
             match Importexport.sr_of_req ~__context req with
             | Some _ -> query (* There was already an SR specified *)
             | None ->
               let pool = Db.Pool.get_all ~__context |> List.hd in
               let default_SR = Db.Pool.get_default_SR ~__context ~self:pool in
               ("sr_id",Ref.string_of default_SR)::query
           in
           let subtask = Client.Task.create rpc session_id "VDI upload" "" in
           Stdext.Pervasiveext.finally (fun () ->
               let req = Http.Request.{
                   req with
                   cookie = strip req.cookie;
                   query = ("task_id",Ref.string_of subtask) :: strip req.query |> add_sr;
                 } in
               let vdi_opt = Import_raw_vdi.localhost_handler rpc session_id (Importexport.vdi_of_req ~__context req) req s in
               match vdi_opt with
               | Some vdi ->
                 begin
                   try
                     let update = Client.Pool_update.introduce rpc session_id vdi in
                     let patch = pool_patch_of_update ~__context update in
                     Db.Task.set_result ~__context ~self:(Context.get_task_id __context) ~value:(Ref.string_of patch);
                     TaskHelper.complete ~__context None
                   with e ->
                     Client.VDI.destroy rpc session_id vdi;
                     TaskHelper.failed ~__context e
                 end
               | None ->
                 (* Propagate the error from the subtask to the main task *)
                 let error_info = Db.Task.get_error_info ~__context ~self:subtask in
                 TaskHelper.failed ~__context Api_errors.(Server_error (List.hd error_info, List.tl error_info));
                 (* If we've got a None here, we'll already have replied with the error. Fail the task now too. *)
                 ())
             (fun () -> Client.Task.destroy rpc session_id subtask)
         )
    )


(* The [get_patch_applied_to] gives the patching status of a pool patch on the given host. It
   returns [None] if the patch is not on the host, i.e. no corresponding host_patch;
   returns [Some (ref, true)] if it's on the host and fully applied (as host_patch [ref]);
   returns [Some (ref, false)] if it's on the host but isn't applied yet or the application is in progress. *)
let get_patch_applied_to ~__context ~patch ~host =
  let expr =
    Db_filter_types.(And (Eq (Field "pool_patch", Literal (Ref.string_of patch)),
                          Eq (Field "host",  Literal (Ref.string_of host))))
  in
  let result = Db.Host_patch.get_records_where ~__context ~expr in
  match result with
  | [] -> None
  | (rf, rc) :: _ -> Some (rf, rc.API.host_patch_applied)


let write_patch_applied_db ~__context ?date ?(applied=true) ~self ~host () =
  let date = Stdext.Date.of_float (match date with
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

(* Helper function. [forward __context self f] finds the update associated
   with the pool_patch reference [self] and applies the function f to that
   update *)
let forward ~__context ~self f =
  let self = Db.Pool_patch.get_pool_update ~__context ~self in
  Helpers.call_api_functions ~__context (fun rpc session_id ->
      f ~rpc ~session_id ~self)

(* precheck API call entrypoint *)
let precheck ~__context ~self ~host =
  ignore (forward ~__context ~self (Client.Pool_update.precheck ~host)); ""

let apply ~__context ~self ~host =
  forward ~__context ~self (Client.Pool_update.apply ~host); ""

let pool_apply ~__context ~self =
  let hosts = (Db.Host.get_all ~__context)
  in
  let (_: string list) =
    List.map
      (fun host ->
         Helpers.call_api_functions ~__context
           (fun rpc session_id -> Client.Pool_patch.apply ~rpc ~session_id ~self ~host)
      )
      hosts
  in
  let _ = Db.Pool_patch.set_pool_applied ~__context ~self ~value:true in
  ()

let clean ~__context ~self =
  ()

let clean_on_host ~__context ~self ~host =
  ()

let pool_clean ~__context ~self =
  forward ~__context ~self Client.Pool_update.pool_clean

let destroy ~__context ~self =
  forward ~__context ~self Client.Pool_update.destroy
