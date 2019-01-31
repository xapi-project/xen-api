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

open Stdext
open Xstringext
open Debug
open Http
open Forkhelpers
open Pervasiveext

exception Failure of string

module D = Debug.Make(struct let name="stream_vdi" end)
open D

(** Inside the tar we divide each VDI into small 'chunk_size' blocks: *)
let chunk_size = Int64.mul 1024L 1024L (* 1 MiB *)

(** Maximum range for sparseness query *)
let max_sparseness_size = Int64.mul 10240L chunk_size

let checksum_extension_xxh = ".xxhash"
let checksum_extension_sha = ".checksum"
let checksum_supported_extension = [checksum_extension_xxh; checksum_extension_sha] (* Supported checksum formats *)

type vdi = string (* directory prefix in tar file *) * API.ref_VDI * Int64.t (* size to send/recieve *)

(** Open the device corresponding to a VDI with <flags> and <perms> and apply the
    resulting file descriptor to <f>. Guarantees to close the file descriptor afterwards. *)
let with_open_vdi __context rpc session_id vdi_ref mode flags perms f =
  Sm_fs_ops.with_block_attached_device __context rpc session_id vdi_ref mode
    (fun dom0_path ->
       debug "with_open_vdi opening: %s" dom0_path;
       let ofd = Unix.openfile dom0_path flags perms in
       Pervasiveext.finally (fun () -> f ofd dom0_path) (fun () -> Unix.close ofd))

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


(** Write a block of checksummed data of length [len] with name [filename] to [ofd] *)
let write_block ~__context filename buffer ofd len =
  let hdr = Tar_unix.Header.make filename (Int64.of_int len) in
  try
    let csum = Printf.sprintf "%016LX" (XXHash.XXH64.hash (Bytes.unsafe_to_string buffer)) in
    Tar_unix.write_block hdr (fun ofd -> Unix.write ofd buffer 0 len |> ignore) ofd;
    (* Write the checksum as a separate file *)
    let hdr' = Tar_unix.Header.make (filename ^ checksum_extension_xxh) (Int64.of_int (String.length csum)) in
    Tar_unix.write_block hdr' (fun ofd -> ignore(Unix.write_substring ofd csum 0 (String.length csum))) ofd
  with
    Unix.Unix_error (a,b,c) as e ->
    TaskHelper.exn_if_cancelling ~__context;
    if b="write"
    then raise (Api_errors.Server_error (Api_errors.client_error, [ExnHelper.string_of_exn e]))
    else raise e

let get_device_numbers path =
  let rdev = (Unix.LargeFile.stat path).Unix.LargeFile.st_rdev in
  let major = rdev / 256 and minor = rdev mod 256 in
  (major, minor)

let is_nbd_device path =
  let nbd_device_num = 43 in
  let (major, _) = get_device_numbers path in
  major = nbd_device_num

type nbd_connect_info =
  { path : string
  ; exportname : string
  } [@@deriving rpc]

let get_nbd_device path =
  let nbd_device_prefix = "/dev/nbd" in
  if Astring.String.is_prefix ~affix:nbd_device_prefix path && is_nbd_device path then begin
    let nbd_number =
      Astring.String.with_range ~first:(String.length nbd_device_prefix) path
    in
    let { path; exportname } =
      (* persistent_nbd_info_dir is written from nbd_client_manager.py as part of VBD plug*)
      let persistent_nbd_info_dir = "/var/run/nonpersistent/nbd" in
      let filename = persistent_nbd_info_dir ^ "/" ^ nbd_number in
      Xapi_stdext_unix.Unixext.string_of_file filename
      |> Jsonrpc.of_string
      |> nbd_connect_info_of_rpc
    in
    Some (path, exportname)
  end else None

type extent = {
  flags : int32;
  length : int64;
} [@@deriving rpc]

type extent_list = extent list [@@deriving rpc]

(* Flags for extents returned for the base:allocation NBD metadata context,
   documented at https://github.com/NetworkBlockDevice/nbd/blob/master/doc/proto.md *)
let flag_hole = 1l
let flag_zero = 2l

(** [descriptor_list] should be a list of non-overlapping extents, ordered from
    lowest offset to highest.
    [offset] is the current offset needed to translate from relative extents to 
    absolute chunk numbers *)
let get_chunk_numbers_in_increasing_order descriptor_list offset =
  (* Output increasing range includes start and end points *)
  let rec range acc start_chunk end_chunk =
    if end_chunk < start_chunk then acc
    else range (end_chunk::acc) start_chunk Int64.(add end_chunk minus_one)
  in

  let is_empty e =
    let has_flag flag =
      Int32.logand e.flags flag = flag
    in
    (* We assume the destination is prezeroed, so we do not have to copy zeroed extents *)
    (has_flag flag_hole) || (has_flag flag_zero)
  in

  (* Output increasing chunks numbers covered by this descriptor if it is not empty *)
  let get_non_empty_chunks descriptor offset increment =
    match (is_empty descriptor) with
      | false ->
        let start_chunk = Int64.div offset increment in
        (* If the chunk ends at the boundary don't include the next chunk *)
        let end_chunk = Int64.(div (add (add descriptor.length offset) minus_one) increment) in
        let chunks = range [] start_chunk end_chunk in
        chunks, descriptor.length
      | true -> [], descriptor.length
  in

  (* Only compare to most recent chunk added *)
  let add_new acc b =
    match acc with
    | hd :: _ when Int64.equal b hd -> acc
    | _ -> b::acc
  in

  (* Output decreasing chunk numbers, x should be increasing and acc should be decreasing *)
  let rec add_unique_chunks acc = function
    | [] -> acc
    | x::xs -> add_unique_chunks (add_new acc x) xs
  in

  (* This works in reverse order *)
  let rec process acc offset = function
    | [] -> acc
    | x::xs ->
      let chunks, add_offset = get_non_empty_chunks x offset chunk_size in
      process (add_unique_chunks acc chunks) (Int64.add offset add_offset) xs
  in

  let chunks = process [] offset descriptor_list  in
  List.rev chunks


(** Stream a set of VDIs split into chunks in a tar format in a defined order. Return an
    association list mapping tar filename -> string (containing the SHA1 checksums) *)
let send_all refresh_session ofd ~__context rpc session_id (prefix_vdis: vdi list) =
  TaskHelper.set_cancellable ~__context;

  let progress = new_progress_record __context prefix_vdis in

  let send_one ofd (__context:Context.t) (prefix, vdi_ref, size) =
    let size = Db.VDI.get_virtual_size ~__context ~self:vdi_ref in
    let reusable_buffer =  Bytes.make (Int64.to_int chunk_size) '\000' in

    with_open_vdi __context rpc session_id vdi_ref `RO [Unix.O_RDONLY] 0o644
    (fun ifd dom0_path ->
      (match get_nbd_device dom0_path with
      | None ->
        (* Remember when we last wrote something so that we can work around firewalls which close 'idle' connections *)
        let last_transmission_time = ref 0. in
        (* NB. It used to be that chunks could be larger than a native int *)
        (* could handle, but this is no longer the case! Ensure all chunks *)
        (* are strictly less than 2^30 bytes *)
        let rec stream_from (chunk_no: int) (offset: int64) =
          refresh_session ();
          let remaining = Int64.sub size offset in
          if remaining > 0L
          then
            begin
              let this_chunk = (min remaining chunk_size) in
              let last_chunk = this_chunk = remaining in
              let this_chunk = Int64.to_int this_chunk in
              let filename = Printf.sprintf "%s/%08d" prefix chunk_no in

              let now = Unix.gettimeofday () in
              let time_since_transmission = now -. !last_transmission_time in

              (* We always include the first and last blocks *)
              let first_or_last = chunk_no = 0 || last_chunk in

              if time_since_transmission > 5. && not first_or_last then begin
                last_transmission_time := now;
                write_block ~__context filename Bytes.empty ofd 0;
                (* no progress has been made *)
                stream_from (chunk_no + 1) offset
              end else begin
                let buffer = if (Int64.of_int this_chunk) = chunk_size
                  then reusable_buffer
                  else Bytes.make this_chunk '\000'
                in
                Unixext.really_read ifd buffer 0 this_chunk;
                if not (Zerocheck.is_all_zeros (Bytes.unsafe_to_string buffer) this_chunk) || first_or_last then begin
                  last_transmission_time := now;
                  write_block ~__context filename buffer ofd this_chunk;
                end;
                made_progress __context progress (Int64.of_int this_chunk);
                stream_from (chunk_no + 1) (Int64.add offset chunk_size);
              end
            end
        in
        stream_from 0 0L
      | Some (path, exportname) ->
          let last_transmission_time = ref 0L in
          let actually_write_chunk (this_chunk_no: int) (this_chunk_size: int) =
            let buffer = if this_chunk_size = Int64.to_int chunk_size
              then reusable_buffer
              else Bytes.make this_chunk_size '\000'
            in
            let filename = Printf.sprintf "%s/%08d" prefix this_chunk_no in
            Unix.LargeFile.lseek ifd (Int64.mul (Int64.of_int this_chunk_no) chunk_size) Unix.SEEK_SET |> ignore;
            Unixext.really_read ifd buffer 0 this_chunk_size;
            last_transmission_time := Mtime_clock.now_ns ();
            write_block ~__context filename buffer ofd this_chunk_size;
            made_progress __context progress (Int64.of_int this_chunk_size);
          in
          let rec stream_from_offset (offset: int64) =
            let remaining = Int64.sub size offset in
            if remaining > 0L then begin
              let this_chunk_size = min (Int64.to_int chunk_size) (Int64.to_int remaining) in
              let this_chunk_no = Int64.div offset chunk_size in
              let now = Mtime_clock.now_ns () in
              let time_since_transmission = Int64.sub now !last_transmission_time in
              if offset = 0L || remaining <= chunk_size || time_since_transmission > 5000000000L then begin
                actually_write_chunk (Int64.to_int this_chunk_no) this_chunk_size;
                stream_from_offset (Int64.add offset (Int64.of_int this_chunk_size))
              end else begin
                let remaining = Int64.mul (Int64.div (Int64.sub remaining 1L) chunk_size) chunk_size in
                (* Get sparseness for next 10GB or until the end rounded by last chunk, whichever is smaller *)
                let sparseness_size = min max_sparseness_size remaining in
                let output, _ = Forkhelpers.execute_command_get_output Xapi_globs.get_nbd_extents ["--path"; path; "--exportname"; exportname; "--offset"; Int64.to_string offset; "--length"; Int64.to_string sparseness_size] in
                let extents = extent_list_of_rpc (Jsonrpc.of_string output) in
                let chunks = get_chunk_numbers_in_increasing_order extents offset in
                List.iter (
                  fun chunk ->
                    actually_write_chunk (Int64.to_int chunk) (Int64.to_int chunk_size);
                  ) chunks;
                stream_from_offset (Int64.add offset sparseness_size)
              end
            end
          in
          stream_from_offset 0L)
      );
    debug "Finished streaming VDI" in
  for_each_vdi __context (send_one ofd __context) prefix_vdis

exception Invalid_checksum of string

(* Rio GA and later only *)
let verify_inline_checksum ifd checksum_table hdr =
  let file_name = hdr.Tar_unix.Header.file_name in
  let length = hdr.Tar_unix.Header.file_size in
  if not(List.exists (Filename.check_suffix file_name) checksum_supported_extension) then begin
    let msg = Printf.sprintf "Expected to find a supported inline checksum file, instead found a file called: %s" file_name in
    error "%s" msg;
    raise (Failure msg)
  end;
  try
    let length' = Int64.to_int length in
    let csum = Bytes.make length' ' ' in
    Unixext.really_read ifd csum 0 length';
    let csum = Bytes.unsafe_to_string csum in
    Tar_unix.Archive.skip ifd (Tar_unix.Header.compute_zero_padding_length hdr);
    (* Look up the relevant file_name in the checksum_table *)
    let original_file_name = Filename.remove_extension file_name in
    let csum' = List.assoc original_file_name !checksum_table in
    if csum <> csum' then begin
      error "File %s checksum mismatch (%s <> %s)" original_file_name csum csum';
      raise (Invalid_checksum (Printf.sprintf "Block %s checksum failed: original = %s; recomputed = %s" original_file_name csum csum'));
    end
  with e ->
    error "Error validating checksums on import: %s" (ExnHelper.string_of_exn e);
    raise e

(** Receive a set of VDIs split into chunks in a tar format in a defined order *)
let recv_all_vdi refresh_session ifd (__context:Context.t) rpc session_id ~has_inline_checksums ~force prefix_vdis =
  TaskHelper.set_cancellable ~__context;

  let progress = new_progress_record __context prefix_vdis in

  let checksum_table = ref [] in

  let firstchunklength = ref (-1) in
  let zerochunkstring = ref Bytes.empty in

  let recv_one ifd (__context:Context.t) (prefix, vdi_ref, size) =
    let vdi_skip_zeros = not (Sm_fs_ops.must_write_zeroes_into_new_vdi ~__context vdi_ref) in
    (* If this is true, we skip writing zeros. Only for sparse files (vhd only atm) *)
    debug "begun import of VDI%s preserving sparseness" (if vdi_skip_zeros then "" else " NOT");

    with_open_vdi __context rpc session_id vdi_ref `RW [Unix.O_WRONLY] 0o644
      (fun ofd dom0_path ->
         let reusable_buffer = Bytes.make (Int64.to_int chunk_size) '\000' in

         let rec stream_from (last_suffix: string) (offset: int64) =
           refresh_session ();

           let remaining = Int64.sub size offset in
           if remaining > 0L
           then begin
             let hdr = Tar_unix.Header.get_next_header ifd in
             let file_name = hdr.Tar_unix.Header.file_name in
             let length = hdr.Tar_unix.Header.file_size in

             (* First chunk will always be there *)
             if !firstchunklength < 0
             then
               begin
                 firstchunklength := (Int64.to_int length);
                 zerochunkstring := Bytes.make !firstchunklength '\000'
               end;

             if not(String.startswith prefix file_name) then begin
               error "Expected VDI chunk prefixed %s; got %s" prefix file_name;
               raise (Failure "Invalid XVA file");
             end;

             (* add one to strip off the '/' from the filename *)
             let suffix = String.sub file_name (1 + String.length prefix) (String.length file_name - (String.length prefix) - 1) in

             if suffix <= last_suffix then begin
               error "Expected VDI chunk suffix to have increased under lexicograpic ordering; last = %s; this = %s" last_suffix suffix;
               raise (Failure "Invalid XVA file")
             end;

             (* Here we find the number of skipped blocks *)
             let num_zero_blocks = (int_of_string suffix) - (int_of_string last_suffix) - 1 in
             let skipped_size = Int64.mul (Int64.of_int !firstchunklength) (Int64.of_int num_zero_blocks) in
             if (num_zero_blocks > 0) then
               begin
                 debug "suffix=%s last_suffix=%s" suffix last_suffix;
                 if vdi_skip_zeros then
                   (* If we're skipping zeros, seek to the correct place *)
                   ignore(Unix.LargeFile.lseek ofd skipped_size Unix.SEEK_CUR)
                 else
                   (* Write some blocks of zeros *)
                   for i=1 to num_zero_blocks do
                     ignore(Unix.write ofd !zerochunkstring 0 (!firstchunklength))
                   done
               end;

             let buffer = if length = chunk_size
               then reusable_buffer
               else Bytes.make (Int64.to_int length) '\000'
             in
             Unixext.really_read ifd buffer 0 (Int64.to_int length);
             Unix.write ofd buffer 0 (Int64.to_int length) |> ignore;
             
             let buffer_string = Bytes.unsafe_to_string buffer in
             let csum_hdr = Tar_unix.Header.get_next_header ifd in (* Header of the checksum file *)
             let csum_file_name = csum_hdr.Tar_unix.Header.file_name in
	     	 
	     	 let csum = (* Infer checksum algorithm from the file format *)
			 	match Filename.extension csum_file_name with
				| m when m = checksum_extension_xxh -> Printf.sprintf "%016LX" (XXHash.XXH64.hash buffer_string)
				| m when m = checksum_extension_sha -> Sha1.to_hex (Sha1.string buffer_string)
				| _ -> begin
						let msg = Printf.sprintf "Found unsupported checksum file: %s" csum_file_name in
						error "%s" msg;
						raise (Failure msg)
					end
			 in

             checksum_table := (file_name, csum) :: !checksum_table;

             Tar_unix.Archive.skip ifd (Tar_unix.Header.compute_zero_padding_length hdr);
             made_progress __context progress (Int64.add skipped_size length);

             if has_inline_checksums then
               begin
                 try
                   verify_inline_checksum ifd checksum_table csum_hdr;
                 with
                 | Invalid_checksum s as e ->
                   if not(force) then raise e
               end;

             stream_from suffix (Int64.add skipped_size (Int64.add offset length))
           end in
         stream_from "-1" 0L;
         Unixext.fsync ofd) in
  begin try
      for_each_vdi __context (recv_one ifd __context) prefix_vdis;
    with Unix.Unix_error(Unix.EIO, _, _) ->
      raise (Api_errors.Server_error (Api_errors.vdi_io_error, ["Device I/O error"]))
  end;
  !checksum_table

let recv_all refresh_session ifd (__context:Context.t) rpc session_id vsn force =
  let has_inline_checksums = vsn.Importexport.export_vsn > 0 in
  recv_all_vdi refresh_session ifd __context rpc session_id ~has_inline_checksums ~force


(** Receive a set of VDIs split into chunks in a tar format created out of a Zurich/Geneva
    exported VM. Each chunk has been independently compressed.*)
let recv_all_zurich refresh_session ifd (__context:Context.t) rpc session_id prefix_vdis =
  TaskHelper.set_cancellable ~__context;
  TaskHelper.set_description ~__context "Importing Virtual Machine";

  let progress = new_progress_record __context prefix_vdis in

  (* The next header in the sequence *)
  let hdr = ref None in
  let next () = hdr := (try Some(Tar_unix.Header.get_next_header ifd) with Tar_unix.Header.End_of_stream -> None | e -> raise e) in
  next();

  let recv_one ifd (__context:Context.t) (prefix, vdi_ref, size) =
    (* Open this VDI and stream in all the blocks. Return when hdr represents
       a chunk which is not part of this VDI or the end of stream is reached. *)
    with_open_vdi __context rpc session_id vdi_ref `RW [Unix.O_WRONLY] 0o644
      (fun ofd dom0_path ->
         let rec stream_from (last_suffix: string) = match !hdr with
           | Some hdr ->
             refresh_session ();

             let file_name = hdr.Tar_unix.Header.file_name in
             let length = hdr.Tar_unix.Header.file_size in
             if String.startswith prefix file_name then begin
               let suffix = String.sub file_name (String.length prefix) (String.length file_name - (String.length prefix)) in
               if suffix <= last_suffix then begin
                 error "Expected VDI chunk suffix to have increased under lexicograpic ordering; last = %s; this = %s" last_suffix suffix;
                 raise (Failure "Invalid XVA file")
               end;
               debug "Decompressing %Ld bytes from %s\n" length file_name;
               Gzip.decompress ofd (fun zcat_in -> Tar_unix.Archive.copy_n ifd zcat_in length);
               Tar_unix.Archive.skip ifd (Tar_unix.Header.compute_zero_padding_length hdr);
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
