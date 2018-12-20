(*
 * Copyright (C) 2006-2013 Citrix Systems Inc.
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
(** HTTP handler for exporting a raw VDI.
 * @group Import and Export
*)

module D = Debug.Make(struct let name="export_raw_vdi" end)
open D

let localhost_handler rpc session_id vdi (req: Http.Request.t) (s: Unix.file_descr) =
  req.Http.Request.close <- true;
  Xapi_http.with_context "Exporting raw VDI" req s
    (fun __context ->
       let task_id = Context.get_task_id __context in
       match Importexport.Format.of_req req with
       | `Unknown x ->
         error "export_raw_vdi task_id = %s; vdi = %s; unknown disk format = %s"
           (Ref.string_of task_id) (Ref.string_of vdi) x;
         TaskHelper.failed ~__context (Api_errors.Server_error(Api_errors.internal_error, ["Unknown format " ^ x]));
         Http_svr.headers s (Http.http_404_missing ~version:"1.0" ())
       | `Ok format ->
         (* Suggest this filename to the client: *)
         let filename = Importexport.Format.filename ~__context vdi format in
         let content_type = Importexport.Format.content_type format in
         debug "export_raw_vdi task_id = %s; vdi = %s; format = %s; content-type = %s; filename = %s"
           (Ref.string_of task_id) (Ref.string_of vdi) (Importexport.Format.to_string format) content_type filename;
         let copy base_path path =
           try
             debug "Copying VDI contents...";
             Vhd_tool_wrapper.send ?relative_to:base_path (Vhd_tool_wrapper.update_task_progress __context)
               "none" (Importexport.Format.to_string format) s path "";
             debug "Copying VDI complete.";
           with Unix.Unix_error(Unix.EIO, _, _) ->
             raise (Api_errors.Server_error (Api_errors.vdi_io_error, ["Device I/O errors"])) in
         begin
           try
             let headers = Http.http_200_ok ~keep_alive:false () @ [
                 Http.Hdr.task_id ^ ":" ^ (Ref.string_of task_id);
                 Http.Hdr.content_type ^ ":" ^ content_type;
                 Http.Hdr.content_disposition ^ ": attachment; filename=\"" ^ filename ^ "\""
               ] in
             Http_svr.headers s headers;
             match format with
             | Raw | Vhd ->
               if format = Vhd then begin
                 let size = Db.VDI.get_virtual_size ~__context ~self:vdi in
                 if size > Constants.max_vhd_size
                 then raise (Api_errors.Server_error(Api_errors.vdi_too_large, [ Ref.string_of vdi; Int64.to_string Constants.max_vhd_size]))
               end;

               Sm_fs_ops.with_block_attached_device __context rpc session_id vdi `RO
                 (fun path ->
                    match Importexport.base_vdi_of_req ~__context req with
                    | Some base_vdi ->
                      Sm_fs_ops.with_block_attached_device __context rpc session_id base_vdi `RO
                        (fun base_path -> copy (Some base_path) path)
                    | None -> copy None path
                 )
             | Tar ->
               (* We need to keep refreshing the session to avoid session timeout *)
               let refresh_session = Xapi_session.consider_touching_session rpc session_id in
               (* The third element of the tuple, the size, is currently ignored by the function *)
               let size = Client.Client.VDI.get_virtual_size ~rpc ~session_id ~self:vdi in
               Stream_vdi.send_all refresh_session s __context rpc session_id [(Xapi_globs.vdi_tar_export_dir, vdi, size)];
               Tar_unix.write_end s;
           with e ->
             Backtrace.is_important e;
             TaskHelper.failed ~__context e;
             raise e
         end
    )

let export_raw vdi (req: Http.Request.t) (s: Unix.file_descr) _ =
  (* Check the SR is reachable (in a fresh task context) *)
  Server_helpers.exec_with_new_task "VDI.export_raw_vdi"
    (fun __context ->
       Helpers.call_api_functions ~__context
         (fun rpc session_id ->
            let sr = Db.VDI.get_SR ~__context ~self:vdi in
            debug "Checking whether localhost can see SR: %s" (Ref.string_of sr);
            if (Importexport.check_sr_availability ~__context sr)
            then localhost_handler rpc session_id vdi req s
            else
              let host = Importexport.find_host_for_sr ~__context sr in
              let address = Db.Host.get_address ~__context ~self:host in
              Importexport.return_302_redirect req s address
         )
    )

let handler (req: Http.Request.t) (s: Unix.file_descr) _  =
  debug "export_raw_vdi handler";
  Xapi_http.assert_credentials_ok "VDI.export_raw" ~http_action:"get_export_raw_vdi" req s;

  Server_helpers.exec_with_new_task "VDI.export_raw_vdi"
    (fun __context ->
       match Importexport.vdi_of_req ~__context req with
       | Some vdi ->
         export_raw vdi req s ()
       | None ->
         failwith "Missing vdi query parameter"
    )
