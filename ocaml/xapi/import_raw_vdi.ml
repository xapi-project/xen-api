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
(** HTTP handler for importing a raw VDI.
 * @group Import and Export
*)

module D=Debug.Make(struct let name="import" end)
open D

open Stdext
open Http
open Importexport
open Sparse_encoding
open Unixext
open Pervasiveext
open Client

let fail_task_in_request (req: Request.t) (s: Unix.file_descr) e =
  ignore(Xapi_http.ref_param_of_req req "task_id"|> Stdext.Opt.map (fun task_id -> TaskHelper.failed ~__context:(Context.from_forwarded_task task_id) e));
  Http_svr.headers s (Http.http_400_badrequest ())

exception HandleError of exn * (string list) (* Exception to put into the task * headers to return to the client *)

let localhost_handler rpc session_id vdi_opt (req: Request.t) (s: Unix.file_descr) =
  req.Request.close <- true;
  Xapi_http.with_context "Importing raw VDI" req s
    (fun __context ->
       let all = req.Request.query @ req.Request.cookie in
       let chunked = List.mem_assoc "chunked" all in
       let task_id = Context.get_task_id __context in
       let format = Importexport.Format.of_req req in
       try
         match format with
         | `Unknown x ->
           error "import_raw_vdi task_id = %s; vdi = %s; unknown disk format = %s"
             (Ref.string_of task_id) (match vdi_opt with Some vdi -> Ref.string_of vdi | None -> "None") x;
           raise (HandleError(Api_errors.Server_error(Api_errors.internal_error, ["Unknown format " ^ x]),Http.http_404_missing ~version:"1.0" ()))
         | `Ok format when format <> Importexport.Format.Raw && chunked ->
           error "import_raw_vdi task_id = %s; vdi = %s; unable to import a .vhd using chunked encoding"
             (Ref.string_of task_id) (match vdi_opt with Some vdi -> Ref.string_of vdi | None -> "None");
           raise (HandleError(Api_errors.Server_error(Api_errors.internal_error, ["Cannot handle chunked VHD"]),Http.http_404_missing ~version:"1.0" ()))
         | `Ok format ->
           debug "import_raw_vdi task_id = %s vdi = %s; chunked = %b; format = %s"
             (Ref.string_of task_id) (match vdi_opt with Some vdi -> Ref.string_of vdi | None -> "None") chunked (Importexport.Format.to_string format);
           match req.Request.transfer_encoding with
           | Some x ->
             error "Chunked encoding not yet implemented in the import code";
             raise (HandleError(Api_errors.Server_error(Api_errors.internal_error, ["Cannot handle encoding: " ^ x]),Http.http_404_missing ~version:"1.0" ()))
           | None ->
             let vdi = match vdi_opt, format, req.Request.content_length, (sr_of_req ~__context req) with
               | Some vdi, _, _, _ -> vdi
               | None, Importexport.Format.Raw, Some length, Some sr ->
                 Client.VDI.create ~rpc ~session_id ~name_label:"Imported VDI" ~name_description:""
                   ~sR:sr ~virtual_size:length ~_type:`user ~sharable:false ~read_only:false
                   ~other_config:[] ~xenstore_data:[] ~sm_config:[] ~tags:[]
               | None, Importexport.Format.Vhd, _, _ ->
                 error "Importing a VHD directly into an SR not yet supported";
                 raise (HandleError(Api_errors.Server_error(Api_errors.internal_error, ["Importing a VHD directly into an SR not yet supported"]),Http.http_400_badrequest ~version:"1.0" ()))
               | _ ->
                 error "Not enough info supplied to import";
                 raise (HandleError(Api_errors.Server_error(Api_errors.internal_error, ["Not enough info supplied to import"]),Http.http_400_badrequest ~version:"1.0" ()))
             in
             let headers = Http.http_200_ok ~keep_alive:false () @
                           [ Http.Hdr.task_id ^ ":" ^ (Ref.string_of task_id);
                             content_type ] in
             Http_svr.headers s headers;
             begin match format with
               | Raw | Vhd ->
                 let prezeroed = not (Sm_fs_ops.must_write_zeroes_into_new_vdi ~__context vdi) in
                 Sm_fs_ops.with_block_attached_device __context rpc session_id vdi `RW
                   (fun path ->
                      if chunked
                      then Vhd_tool_wrapper.receive (Vhd_tool_wrapper.update_task_progress __context) "raw" "chunked" s None path "" prezeroed
                      else Vhd_tool_wrapper.receive (Vhd_tool_wrapper.update_task_progress __context) (Importexport.Format.to_string format) "none" s req.Request.content_length path "" prezeroed
                   );
               | Tar ->
                 (* We need to keep refreshing the session to avoid session timeout *)
                 let refresh_session = Xapi_session.consider_touching_session rpc session_id in
                 let size = Client.VDI.get_virtual_size ~rpc ~session_id ~self:vdi in
                 (* VDIs exported as TAR archives will always have inline checksums *)
                 Stream_vdi.recv_all_vdi refresh_session s __context rpc session_id ~has_inline_checksums:true ~force:false [(Xapi_globs.vdi_tar_export_dir, vdi, size)] |> ignore;
             end;
             TaskHelper.complete ~__context (Some (API.rpc_of_ref_VDI vdi));
             Some vdi
       with
       | HandleError (exn, headers) ->
         TaskHelper.failed ~__context exn;
         Http_svr.headers s headers;
         None
       | e ->
         Backtrace.is_important e;
         error "Caught exception: %s" (ExnHelper.string_of_exn e);
         TaskHelper.failed ~__context e;
         raise e
    )

let import vdi (req: Request.t) (s: Unix.file_descr) _ =
  Xapi_http.assert_credentials_ok "VDI.import" ~http_action:"put_import_raw_vdi" req s;

  (* Perform the SR reachability check using a fresh context/task because
     	   we don't want to complete the task in the forwarding case *)

  Server_helpers.exec_with_new_task "VDI.import"
    (fun __context ->
       try
         Helpers.call_api_functions ~__context
           (fun rpc session_id ->
              let sr_opt = match vdi, sr_of_req ~__context req with
                | Some vdi, _ -> Some (Db.VDI.get_SR ~__context ~self:vdi)
                | None, Some sr -> Some sr
                | None, None -> None
              in
              match sr_opt with
              | Some sr ->
                debug "Checking whether localhost can see SR: %s" (Ref.string_of sr);
                if (Importexport.check_sr_availability ~__context sr)
                then localhost_handler rpc session_id vdi req s
                else
                  let host = Importexport.find_host_for_sr ~__context sr in
                  let address = Db.Host.get_address ~__context ~self:host in
                  return_302_redirect req s address;
                  None
              | None ->
                error "Require an SR or VDI to import";
                fail_task_in_request req s Api_errors.(Server_error(vdi_missing,[]));
                None
           )
       with e ->
         error "Caught exception in import handler: %s" (ExnHelper.string_of_exn e);
         fail_task_in_request req s e;
         raise e

    )


let handler (req: Request.t) (s: Unix.file_descr) _ =
  Xapi_http.assert_credentials_ok "VDI.import" ~http_action:"put_import_raw_vdi" req s;

  (* Using a fresh context/task because we don't want to complete the
     	   task in the forwarding case *)
  Server_helpers.exec_with_new_task "VDI.import"
    (fun __context ->
       ignore(import (vdi_of_req ~__context req) req s ())
    )
