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
(** Utility functions for streaming VDI images
 * @group Storage
 *)

open Stringext
open Debug
open Http
open Forkhelpers
open Pervasiveext

exception Failure of string

module D = Debug.Make(struct let name="stream_vdi" end)
open D

(** Inside the tar we divide each VDI into small 'chunk_size' blocks: *)
let chunk_size = Int64.mul 1024L 1024L (* 1 MiB *)  

let checksum_extension = ".checksum"

(** Helper function to prevent double-closes of file descriptors *)
let close to_close fd = 
  if List.mem fd !to_close then Unix.close fd;
  to_close := List.filter (fun x -> fd <> x) !to_close 

type vdi = string (* directory prefix in tar file *) * API.ref_VDI * Int64.t (* size to send/recieve *)

(** Open the device corresponding to a VDI with <flags> and <perms> and apply the 
    resulting file descriptor to <f>. Guarantees to close the file descriptor afterwards. *)
let with_open_vdi __context rpc session_id vdi_ref mode flags perms f =
  Sm_fs_ops.with_block_attached_device __context rpc session_id vdi_ref mode
    (fun dom0_path ->
       debug "with_open_vdi opening: %s" dom0_path;
       let ofd = Unix.openfile dom0_path flags perms in  
       Pervasiveext.finally (fun () -> f ofd) (fun () -> Unix.close ofd))

(** Used to sort VDI prefixes into a canonical order for streaming. Currently lexicographic
    sort on the externalised reference (used as a 'directory name') *)
let vdi_ordering (a,_,_) (b,_,_) = compare a b

(** Lock a bunch of VDIs and then call <f> with them sorted via <vdi_ordering> *)
(** the function 'f' is responsible for doing attach/activate then deactivate/detach at the end *)
let for_each_vdi __context f prefix_vdis =
  let sorted_prefix_vdis = List.sort vdi_ordering prefix_vdis in
  List.iter f sorted_prefix_vdis

(** Represent the progress made streaming a set of disks *)
type progress = 
    { total_size: int64;
      mutable transmitted_so_far: int64;
      mutable time_of_last_update: float;
      __context: Context.t
    }

(** Create a fresh progress record from a set of VDIs *)
let new_progress_record __context (prefix_vdis: vdi list) = 
  { total_size = List.fold_left (fun tot (_,_,s) -> Int64.add tot s) 0L prefix_vdis;
    transmitted_so_far = 0L;
    time_of_last_update = 0.0;
    __context = __context }

(** Called every time <n> (uncompressed) bytes have been read or written. Updates
    the task record in the database if no update has been sent for 10 seconds. *)
let made_progress __context progress n = 
  let total_size_MiB = Int64.div progress.total_size 1048576L in
  progress.transmitted_so_far <- Int64.add progress.transmitted_so_far n;
  let so_far_MiB = Int64.div progress.transmitted_so_far 1048576L in
  let fraction_complete = Int64.to_float so_far_MiB /. (Int64.to_float total_size_MiB) in
  let now = Unix.time () in
  let time_since_last_update = now -. progress.time_of_last_update in
  if time_since_last_update > 10.0 then begin
    progress.time_of_last_update <- now;
    TaskHelper.set_progress progress.__context fraction_complete;
    TaskHelper.exn_if_cancelling ~__context;
  end


(** Stream a set of VDIs split into chunks in a tar format in a defined order. *)
let send_all refresh_session ofd ~__context rpc session_id (prefix_vdis: vdi list) = 
  TaskHelper.set_cancellable ~__context;

  let progress = new_progress_record __context prefix_vdis in

  let progress_cb size =
    let last_update = ref 0L in
    fun percent_complete ->
      refresh_session ();
      let transmitted = Int64.(div (mul size (of_int percent_complete)) 100L) in
      made_progress __context progress (Int64.sub transmitted !last_update);
      last_update := transmitted in


  let send_one ofd (__context:Context.t) (prefix, vdi_ref, size) =
    Sm_fs_ops.with_block_attached_device __context rpc session_id vdi_ref `RO
      (fun path ->
        (* XXX: cancellation *)
        Vhd_tool_wrapper.send (progress_cb size) ofd path prefix
      ) in
  for_each_vdi __context (send_one ofd __context) prefix_vdis

(** Receive a set of VDIs split into chunks in a tar format in a defined order *)
let recv_all refresh_session ifd (__context:Context.t) rpc session_id vsn force prefix_vdis = 
  TaskHelper.set_cancellable ~__context;

  let progress = new_progress_record __context prefix_vdis in

  let progress_cb size =
    let last_update = ref 0L in
    fun percent_complete ->
      refresh_session ();
      let transmitted = Int64.(div (mul size (of_int percent_complete)) 100L) in
      made_progress __context progress (Int64.sub transmitted !last_update);
      last_update := transmitted in

  let recv_one ifd (__context:Context.t) (prefix, vdi_ref, size) =
    let prezeroed = not (Sm_fs_ops.must_write_zeroes_into_new_vdi ~__context vdi_ref) in
    Sm_fs_ops.with_block_attached_device __context rpc session_id vdi_ref `RO
      (fun path ->
        (* XXX: cancellation *)
        Vhd_tool_wrapper.receive (progress_cb size) "tar" ifd path prefix prezeroed
      ) in
  for_each_vdi __context (recv_one ifd __context) prefix_vdis

(** Receive a set of VDIs split into chunks in a tar format created out of a Zurich/Geneva
    exported VM. Each chunk has been independently compressed.*)
let recv_all_zurich refresh_session ifd (__context:Context.t) rpc session_id prefix_vdis = 
  TaskHelper.set_cancellable ~__context;
  TaskHelper.set_description ~__context "Importing Virtual Machine";

  let progress = new_progress_record __context prefix_vdis in

  (* The next header in the sequence *)
  let hdr = ref None in
  let next () = hdr := (try Some(Tar.Header.get_next_header ifd) with Tar.Header.End_of_stream -> None | e -> raise e) in
  next();
  
  let recv_one ifd (__context:Context.t) (prefix, vdi_ref, size) =
    (* Open this VDI and stream in all the blocks. Return when hdr represents
       a chunk which is not part of this VDI or the end of stream is reached. *)
    with_open_vdi __context rpc session_id vdi_ref `RW [Unix.O_WRONLY] 0o644
      (fun ofd ->
	 let rec stream_from (last_suffix: string) = match !hdr with
	   | Some hdr ->
	       refresh_session ();

	       let file_name = hdr.Tar.Header.file_name in
	       let length = hdr.Tar.Header.file_size in
	       if String.startswith prefix file_name then begin
		   let suffix = String.sub file_name (String.length prefix) (String.length file_name - (String.length prefix)) in
		   if suffix <= last_suffix then begin
		       error "Expected VDI chunk suffix to have increased under lexicograpic ordering; last = %s; this = %s" last_suffix suffix;
		       raise (Failure "Invalid XVA file")
		     end;
		   debug "Decompressing %Ld bytes from %s\n" length file_name;
		   Gzip.decompress ofd (fun zcat_in -> Tar.Archive.copy_n ifd zcat_in length);
		   Tar.Archive.skip ifd (Tar.Header.compute_zero_padding_length hdr);
		   (* XXX: this is totally wrong: *)
		   made_progress __context progress length;
		   next ();
		   stream_from suffix
	     end 
	   | None -> 	       
	       (* Since we don't count uncompressed bytes we aren't sure if we've
		  really finished unfortunately. We can at least check to see if we
	          were cancelled... *)
	       TaskHelper.exn_if_cancelling ~__context;
	       () in
	stream_from "";
	Unixext.fsync ofd) in
  begin try
    for_each_vdi __context (recv_one ifd __context) prefix_vdis;
  with Unix.Unix_error(Unix.EIO, _, _) ->
    raise (Api_errors.Server_error (Api_errors.vdi_io_error, ["Device I/O error"]))
  end;
  if !hdr <> None then begin
      error "Failed to import XVA; some chunks were not processed.";
      raise (Failure "Some XVA data not processed")
    end
