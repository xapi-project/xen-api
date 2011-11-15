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

module D = Debug.Debugger(struct let name="xapi_blob" end)
open D

let create ~__context ~mime_type =
  let uuid = Uuid.make_uuid () in
  let ref = Ref.make () in
  let mime_type' = if mime_type="" then "application/octet-stream" else mime_type in
  Db.Blob.create ~__context ~ref ~uuid:(Uuid.to_string uuid) ~mime_type:mime_type' ~size:0L ~last_updated:(Date.never) ~name_label:"" ~name_description:"";
  ref

let destroy ~__context ~self =
  (* This needs to be special-cased for all objects that contain blobs *)
  let vms = Db.VM.get_all_records ~__context in
  List.iter (fun (vm,vmr) ->
    let blobs = vmr.API.vM_blobs in
    List.iter (fun (r,b) -> if b=self then Db.VM.remove_from_blobs ~__context ~self:vm ~key:r) blobs) vms;
  
  let uuid = Db.Blob.get_uuid ~__context ~self in
  let path = Xapi_globs.xapi_blob_location ^ "/" ^ uuid in
  Unixext.unlink_safe path;
  Db.Blob.destroy ~__context ~self

exception Unknown_blob
exception No_storage

let handler (req: Http.Request.t) s _ =
  let query = req.Http.Request.query in
  req.Http.Request.close <- true;
  debug "blob handler";
  if not(List.mem_assoc "ref" query) then begin
    let headers = Http.http_400_badrequest () in
    Http_svr.headers s headers;
    error "HTTP request for binary blob lacked 'ref' parameter";
  end else
    Xapi_http.with_context ~dummy:true "Blob handler" req s
      (fun __context ->
	let self = Ref.of_string (List.assoc "ref" query) in
	try 
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
		    ignore(Pervasiveext.finally 
			      (fun () -> Unixext.copy_file ifd s) 
			      (fun () -> Unix.close ifd))
		  with _ ->
		    Http_svr.headers s (Http.http_404_missing ())
		end
	    | Http.Put ->
		let ofd = Unix.openfile path [Unix.O_WRONLY; Unix.O_TRUNC; Unix.O_SYNC; Unix.O_CREAT] 0o600 in
		let size = 
		  Pervasiveext.finally
		    (fun () -> 
		      Http_svr.headers s (Http.http_200_ok ());
		      Unixext.copy_file s ofd)
		    (fun () -> 
		      Unix.close ofd)
		in
		Db.Blob.set_size ~__context ~self ~value:size;
		Db.Blob.set_last_updated ~__context ~self ~value:(Date.of_float (Unix.gettimeofday ()))
		| _ -> failwith "Unsupported method for BLOB"
	with
	  | Unknown_blob -> 
	      Http_svr.response_missing s "Unknown reference\n"
	  | No_storage ->
	      Http_svr.response_missing s "Local storage missing\n"
      )
