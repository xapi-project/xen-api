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
(** Binary blob management *)

module D = Debug.Make(struct let name="xapi_blob" end)
open D

let create ~__context ~mime_type ~public =
  let uuid = Uuid.make_uuid () in
  let ref = Ref.make () in
  let mime_type' = if mime_type="" then "application/octet-stream" else mime_type in
  Db.Blob.create ~__context ~ref ~uuid:(Uuid.to_string uuid) ~public ~mime_type:mime_type' ~size:0L ~last_updated:(Stdext.Date.never) ~name_label:"" ~name_description:"";
  ref

let destroy ~__context ~self =
  (* This needs to be special-cased for all objects that contain blobs *)
  let vms = Db.VM.get_all_records ~__context in
  List.iter (fun (vm,vmr) ->
      let blobs = vmr.API.vM_blobs in
      List.iter (fun (r,b) -> if b=self then Db.VM.remove_from_blobs ~__context ~self:vm ~key:r) blobs) vms;

  let uuid = Db.Blob.get_uuid ~__context ~self in
  let path = Xapi_globs.xapi_blob_location ^ "/" ^ uuid in
  Stdext.Unixext.unlink_safe path;
  Db.Blob.destroy ~__context ~self

(* Send blobs to a remote host on a different pool. uuid_map is a
   map of remote blob uuids to local blob refs. *)
let send_blobs ~__context ~remote_address ~session_id uuid_map =
  let put_blob = function (new_ref, old_uuid) ->
  try
    let query = [ "session_id", Ref.string_of session_id
                ; "ref", Ref.string_of new_ref ] in
    let subtask_of = Context.string_of_task __context in
    let path = Xapi_globs.xapi_blob_location ^ "/" ^ old_uuid in
    let size = (Unix.LargeFile.stat path).Unix.LargeFile.st_size in
    let request = Xapi_http.http_request ~query ~subtask_of ~length:size
        Http.Put Constants.blob_uri in

    let open Xmlrpc_client in
    let transport = SSL(SSL.make (), remote_address,
                        !Xapi_globs.https_port) in
    with_transport transport
      (with_http request (fun (response, put_fd) ->
           let blob_fd = Unix.openfile path [Unix.O_RDONLY] 0o600 in
           ignore (Stdext.Pervasiveext.finally
                     (fun () -> Stdext.Unixext.copy_file blob_fd put_fd)
                     (fun () -> Unix.close blob_fd)) ))
  with e ->
    debug "Ignoring exception in send_blobs: %s" (Printexc.to_string e);
    ()
  in
  List.iter put_blob uuid_map

(* Send a VMs blobs to a remote host on another pool, and destroy the
   leftover blobs on this host. To be called from
   Xapi_vm_migrate.migrate. *)
let migrate_push ~__context ~rpc ~remote_address ~session_id ~old_vm ~new_vm =
  let vm_blobs = Db.VM.get_blobs ~__context ~self:old_vm in
  (* Create new blob objects on remote host, and return a map
     		   from new blob uuids to old blob refs *)
  let uuid_map = List.map
      (fun (name,self) ->
         let mime_type = Db.Blob.get_mime_type ~__context ~self
         and public = Db.Blob.get_public ~__context ~self
         and old_uuid = Db.Blob.get_uuid ~__context ~self in
         let new_ref = Client.Client.VM.create_new_blob
             ~rpc ~session_id ~vm:new_vm ~name ~mime_type ~public in
         let name = Db.Blob.get_name_label ~__context ~self in
         Client.Client.Blob.set_name_label ~rpc ~session_id ~self:new_ref ~value:name;
         (new_ref, old_uuid) )
      vm_blobs
  in
  send_blobs ~__context ~remote_address ~session_id uuid_map ;
  (* Now destroy old blobs *)
  List.iter (fun (_,self) ->
      destroy ~__context ~self:(Db.Blob.get_by_uuid ~__context ~uuid:self))
    uuid_map

exception Unknown_blob
exception No_storage

let handler (req: Http.Request.t) s _ =
  let query = req.Http.Request.query in
  req.Http.Request.close <- true;
  if not(List.mem_assoc "ref" query || List.mem_assoc "uuid" query) then begin
    let headers = Http.http_400_badrequest () in
    Http_svr.headers s headers;
    error "HTTP request for binary blob lacked 'ref' or 'uuid' parameter"
  end else
    try
      let self,public =
        Server_helpers.exec_with_new_task "with_context" (fun __context ->
            let self = try
                Ref.of_string (List.assoc "ref" query)
              with _ ->
              try Db.Blob.get_by_uuid ~__context ~uuid:(List.assoc "uuid" query) with _ -> raise Unknown_blob
            in
            debug "blob handler: self=%s" (Ref.string_of self);
            let public =
              try
                Db.Blob.get_public ~__context ~self
              with e ->
                debug "In exception handler: %s" (Printexc.to_string e);
                false
            in
            debug "public=%b" public;
            self,public)
      in
      let inner_fn __context =
        let blob_uuid =
          try Db.Blob.get_uuid ~__context ~self
          with _ -> raise Unknown_blob
        in
        let blob_path = Xapi_globs.xapi_blob_location in
        (try let (_: Unix.stats) = Unix.stat blob_path in () with _ -> raise No_storage);
        let path = Xapi_globs.xapi_blob_location ^ "/" ^ blob_uuid in

        match req.Http.Request.m with
        | Http.Get ->
          begin
            try
              (* The following might raise an exception, in which case, 404 *)
              let ifd = Unix.openfile path [Unix.O_RDONLY] 0o600 in
              let size = (Unix.LargeFile.stat path).Unix.LargeFile.st_size in
              Http_svr.headers s ((Http.http_200_ok_with_content
                                     size ~version:"1.1" ~keep_alive:false ())
                                  @ [Http.Hdr.content_type ^": "^(Db.Blob.get_mime_type ~__context ~self)]);
              ignore(Stdext.Pervasiveext.finally
                       (fun () -> Stdext.Unixext.copy_file ifd s)
                       (fun () -> Unix.close ifd))
            with _ ->
              Http_svr.headers s (Http.http_404_missing ())
          end
        | Http.Put ->
          let ofd = Unix.openfile path [Unix.O_WRONLY; Unix.O_TRUNC; Unix.O_SYNC; Unix.O_CREAT] 0o600 in
          let limit = match req.Http.Request.content_length with Some x -> x | None -> failwith "Need content length" in
          let size =
            Stdext.Pervasiveext.finally
              (fun () ->
                 Http_svr.headers s (Http.http_200_ok () @ ["Access-Control-Allow-Origin: *"]);
                 Stdext.Unixext.copy_file ~limit s ofd)
              (fun () ->
                 Unix.close ofd)
          in
          Db.Blob.set_size ~__context ~self ~value:size;
          Db.Blob.set_last_updated ~__context ~self ~value:(Stdext.Date.of_float (Unix.gettimeofday ()))
        | _ -> failwith "Unsupported method for BLOB"

      in

      if public && req.Http.Request.m = Http.Get
      then Server_helpers.exec_with_new_task "get_blob" inner_fn
      else Xapi_http.with_context ~dummy:true "Blob handler" req s inner_fn
    with
    | Unknown_blob ->
      Http_svr.response_missing s "Unknown reference\n"
    | No_storage ->
      Http_svr.response_missing s "Local storage missing\n"







