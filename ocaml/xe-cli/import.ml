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
(*
 * CLI support for stand-alone VM import command which can handle XVA directories
 * (the export format of Zurich)
 *)

open Stringext
open Xml

type classification = 
  | Zurich (** directory-style XVA produced by Zurich *)
  | TarXVA (** new single-file XVA produced by this codebase *)
  | Unknown

let classify path =
  try 
    match (Unix.LargeFile.stat path).Unix.LargeFile.st_kind with
    | Unix.S_REG -> TarXVA
    | Unix.S_DIR -> 
	let xml = path ^ "/" ^ Xva.xml_filename in
	begin
	  try
	    Unix.access xml [ Unix.F_OK ];
	    Zurich
	  with Unix.Unix_error _ -> Unknown
	end
    | _ -> Unknown
  with Unix.Unix_error _ as e ->
    debug(Printf.sprintf "Failed to stat: %s" path);
    raise e

(** Stream a string into a tar, with a valid header and padding *)
let stream_file_from_string oc file_name body  = 
  let hdr = Tar.Header.make file_name (Int32.of_int (String.length body)) in
  output_string oc (Tar.Header.marshal hdr);
  output_string oc body;
  output_string oc (Tar.Header.zero_padding hdr)

let end_stream oc = 
  (* Add two empty blocks *)
  output_string oc Tar.Header.zero_block;
  output_string oc Tar.Header.zero_block;
  flush oc
 
(** Upload a Zurich-format XVA directory *)
let stream_from_xva_dir dir oc = 
  let xml = Xml.parse_file (dir ^ "/" ^ Xva.xml_filename) in
  stream_file_from_string oc Xva.xml_filename (Xml.to_string xml);
  let vms, vdis = Xva.of_xml xml in

  List.iter (fun vdi ->
	       if vdi.Xva.ty <> "dir-gzipped-chunks" 
	       then failwith (Printf.sprintf "Cannot handle directory XVA disk encoding: %s" vdi.Xva.ty);

	       let prefix = "file://" in
	       if not(String.startswith prefix vdi.Xva.source)
	       then failwith (Printf.sprintf "Cannot handle disk source: %s (need a file:// URI)" vdi.Xva.source);

	       let suffix = String.sub vdi.Xva.source (String.length prefix) (String.length vdi.Xva.source - (String.length prefix)) in
	       let vdi_dir = dir ^ "/" ^ suffix in
	       let chunks = List.filter (String.startswith "chunk-") (Array.to_list (Sys.readdir vdi_dir)) in
	       if List.length chunks = 0 then failwith (Printf.sprintf "Failed to find disk chunks in dir: %s" vdi_dir);

	       (** Uncompress and stream each of the chunks in series *)
	       let so_far = ref 0L in
	       let total = vdi.Xva.size in
	       let remaining_this_chunk = ref 0L in
	       let chunks = List.sort compare chunks in
	       let chunk_number = ref 0 in

	       (** Construct a new header, add any necessary zero-padding for the previous header
		   and send the new header if size <> 0 *)
	       let make_hdr =
		 let last_hdr = ref None in
		 fun size -> 
		   begin match !last_hdr with
		   | Some hdr -> output_string oc (Tar.Header.zero_padding hdr)
		   | None -> ()
		   end;		 
		   let hdr = Tar.Header.make 
		     (Printf.sprintf "%s/%08d" vdi.Xva.vdi_name !chunk_number) 
		     (Int64.to_int32 size) in
		   incr chunk_number;
		   last_hdr := Some hdr;
		   if size <> 0L
		   then output_string oc (Tar.Header.marshal hdr) in

	       List.iter (fun chunk ->
			    (** Uncompress and stream each of the chunks in series *)
			    let file = vdi_dir ^ "/" ^ chunk in

			    let ic = Unix.open_process_in ("zcat " ^ file) in
			    let finished = ref false in
			    while not(!finished) do

			      if !remaining_this_chunk = 0L then begin
				remaining_this_chunk := min (Int64.sub total !so_far) 1000000L;
				make_hdr !remaining_this_chunk;
			      end;

			      let bytes : int64 = Unix.copy_file ~limit:(Some !remaining_this_chunk) ic oc in

			      finished := bytes < !remaining_this_chunk || bytes = 0L;
			      remaining_this_chunk := Int64.sub !remaining_this_chunk bytes;
			      so_far := Int64.add !so_far bytes;
			    done;
			    close_in ic)  (List.sort compare chunks);
	       if !so_far <> total 
	       then failwith (Printf.sprintf "Streamed all the disk chunks I could find but I count only %s bytes out of %s"
				(Int64.to_string !so_far) (Int64.to_string total))
	    ) vdis;
 end_stream oc

(** Upload from a single file XVA *)		       
let stream_from_xva_file file oc = 
  let ic = open_in_bin file in
  let bytes = Unix.copy_file ic oc in
  close_in ic;
  end_stream oc;
  debug(Printf.sprintf "Finished streaming %s bytes" (Int64.to_string bytes))
