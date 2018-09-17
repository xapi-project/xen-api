(*
 * Copyright (C) 2011-2013 Citrix Inc
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

open Common
open Lwt

external sendfile: Unix.file_descr -> Unix.file_descr -> int64 -> int64 = "stub_sendfile64"

module F = Vhd_format.F.From_file(Vhd_format_lwt.IO)
module In = Vhd_format.F.From_input(Input)
module Channel_In = Vhd_format.F.From_input(struct
  include Lwt
  type fd = Channels.t
  let read c buf = c.Channels.really_read buf
  let scratch = IO.alloc (1024 * 1024)
  let skip_to c offset =
    let rec drop remaining =
      if remaining = 0L
      then return ()
      else
        let this = Int64.(to_int (min (of_int (Cstruct.len scratch)) remaining)) in
        let frag = Cstruct.sub scratch 0 this in
        read c frag >>= fun () ->
        drop Int64.(sub remaining (of_int this)) in
    drop Int64.(sub offset !(c.Channels.offset))
end)
open F
(*
open Vhd
open Vhd_format_lwt
*)
let vhd_search_path = "/dev/mapper"

let require name arg = match arg with
  | None -> failwith (Printf.sprintf "Please supply a %s argument" name)
  | Some x -> x

let get _common filename key =
  try
    let filename = require "filename" filename in
    let key = require "key" key in
    let t =
      Vhd_IO.openfile filename false >>= fun t ->
      let result = Vhd_format.F.Vhd.Field.get t key in
      Vhd_IO.close t >>= fun () ->
      return result in
    match Lwt_main.run t with
    | Some v ->
      Printf.printf "%s\n" v;
      `Ok ()
    | None -> raise Not_found
  with
    | Failure x ->
      `Error(true, x)
    | Not_found ->
      `Error(true, Printf.sprintf "Unknown key. Known keys are: %s" (String.concat ", " Vhd_format.F.Vhd.Field.list))

let info _common filename =
  try
    let filename = require "filename" filename in
    let t =
      Vhd_IO.openfile filename false >>= fun t ->
      let all = List.map (fun f ->
        match Vhd_format.F.Vhd.Field.get t f with
        | Some v -> [ f; v ]
        | None -> [ f; "<missing field>" ]
      ) Vhd_format.F.Vhd.Field.list in
      print_table ["field"; "value"] all;
      return () in
    Lwt_main.run t;
    `Ok ()
  with Failure x ->
    `Error(true, x)

let contents _common filename =
  try
    let filename = require "filename" filename in
    let t =
      let open In in
      Vhd_format_lwt.IO.openfile filename false >>= fun fd ->
      let rec loop = function
      | End -> return ()
      | Cons (hd, tl) ->
        let open Vhd_format.F in
        begin match hd with
        | Fragment.Header _x ->
          Printf.printf "Header\n"
        | Fragment.Footer _x ->
          Printf.printf "Footer\n"
        | Fragment.BAT _x ->
          Printf.printf "BAT\n"
        | Fragment.Batmap _x ->
          Printf.printf "batmap\n"
        | Fragment.Block (offset, buffer) ->
          Printf.printf "Block %Ld (len %d)\n" offset (Cstruct.len buffer)
        end;
        tl () >>= fun x ->
        loop x in
      Vhd_format_lwt.IO.get_file_size filename >>= fun size ->
      openstream (Some size) (Input.of_fd (Vhd_format_lwt.IO.to_file_descr fd)) >>= fun stream ->
      loop stream in
    Lwt_main.run t;
    `Ok ()
  with Failure x ->
    `Error(true, x)

let create common filename size parent =
  try
    begin let filename = require "filename" filename in
    match parent, size with
    | None, None -> failwith "Please supply either a size or a parent"
    | None, Some size ->
      let size = parse_size size in
      let t =
        Vhd_IO.create_dynamic ~filename ~size () >>= fun vhd ->
        Vhd_IO.close vhd in
      Lwt_main.run t
    | Some parent, None ->
      let t =
        Vhd_IO.openchain ~path:common.path parent false >>= fun parent ->
        Vhd_IO.create_difference ~filename ~parent () >>= fun vhd ->
        Vhd_IO.close parent >>= fun () ->
        Vhd_IO.close vhd >>= fun () ->
        return () in
      Lwt_main.run t
    | Some _parent, Some _size ->
      failwith "Overriding the size in a child node not currently implemented"
    end;
     `Ok ()
  with Failure x ->
    `Error(true, x)

let check common filename =
  try
    let filename = require "filename" filename in
    let t =
      Vhd_IO.openchain ~path:common.path filename false >>= fun vhd ->
      Vhd_format.F.Vhd.check_overlapping_blocks vhd;
      return () in
    Lwt_main.run t;
    `Ok ()
  with Failure x ->
    `Error(true, x)

module P = Progress_bar(Int64)

let console_progress_bar total_work =
  let p = P.create 80 0L total_work in
  fun work_done ->
    let progress_updated = P.update p work_done in
    if progress_updated then P.print_bar p;
    if work_done = total_work then begin
      Printf.printf "\n";
      P.summarise p;
      Printf.printf "%!"
    end

let machine_progress_bar total_work =
  let last_percent = ref (-1) in
  fun work_done ->
    let new_percent = Int64.(to_int (div (mul work_done 100L) total_work)) in
    if new_percent <= 100 && !last_percent <> new_percent then begin
      Printf.printf "%03d%!" new_percent;
      last_percent := new_percent
    end

let no_progress_bar _ _ = ()

let [@warning "-27"]
  stream_human _common _ s _ _ ?(progress = no_progress_bar) () =
  let decimal_digits =
    let open Vhd_format.F in
    (* How much space will we need for the sector numbers? *)
    let sectors = Int64.(shift_right (add s.size.total 511L) sector_shift) in
    let decimal_digits = int_of_float (ceil (log10 (Int64.to_float sectors))) in
    Printf.printf "# stream summary:\n";
    Printf.printf "# size of the final artifact: %Ld\n" s.size.total;
    Printf.printf "# size of metadata blocks:    %Ld\n" s.size.metadata;
    Printf.printf "# size of empty space:        %Ld\n" s.size.empty;
    Printf.printf "# size of referenced blocks:  %Ld\n" s.size.copy;
    Printf.printf "# offset : contents\n";
    decimal_digits in
  fold_left (fun sector x ->
    Printf.printf "%s: %s\n"
      (padto ' ' decimal_digits (Int64.to_string sector))
      (Vhd_format.Element.to_string x);
    return (Int64.add sector (Vhd_format.Element.len x))
  ) 0L s.elements >>= fun _ ->
  Printf.printf "# end of stream\n";
  return None

let stream_nbd _common c s prezeroed _ ?(progress = no_progress_bar) () =
  let open Nbd_lwt_unix in
  let c = { Nbd.Channel.read = c.Channels.really_read; write = c.Channels.really_write; close = c.Channels.close; is_tls = false } in

  Client.negotiate c "" >>= fun (server, _size, _flags) ->
  (* Work to do is: non-zero data to write + empty sectors if the
     target is not prezeroed *)
  let total_work = let open Vhd_format.F in Int64.(add (add s.size.metadata s.size.copy) (if prezeroed then 0L else s.size.empty)) in
  let p = progress total_work in

  ( if not prezeroed then expand_empty s else return s ) >>= fun s ->
    expand_copy s >>= fun s ->

  fold_left (fun (sector, work_done) x ->
    ( match x with
      | `Sectors data ->
        Client.write server (Int64.mul sector 512L) [data] >>= begin
          function
          | Ok () -> return Int64.(of_int (Cstruct.len data))
          | Error _e -> fail (Failure "Got error from NBD library")
        end
      | `Empty _n -> (* must be prezeroed *)
        assert prezeroed;
        return 0L
      | _ -> fail (Failure (Printf.sprintf "unexpected stream element: %s" (Vhd_format.Element.to_string x))) ) >>= fun work ->
    let sector = Int64.add sector (Vhd_format.Element.len x) in
    let work_done = Int64.add work_done work in
    p work_done;
    return (sector, work_done)
  ) (0L, 0L) s.elements >>= fun _ ->
  p total_work;

  return (Some total_work)

let stream_chunked _common c s prezeroed _ ?(progress = no_progress_bar) () =
  (* Work to do is: non-zero data to write + empty sectors if the
     target is not prezeroed *)
  let total_work = let open Vhd_format.F in Int64.(add (add s.size.metadata s.size.copy) (if prezeroed then 0L else s.size.empty)) in
  let p = progress total_work in

  ( if not prezeroed then expand_empty s else return s ) >>= fun s ->
  expand_copy s >>= fun s ->

  let header = Cstruct.create Chunked.sizeof in
  fold_left (fun(sector, work_done) x ->
    ( match x with
      | `Sectors data ->
        let t = { Chunked.offset = Int64.(mul sector 512L); data } in
        Chunked.marshal header t;
        c.Channels.really_write header >>= fun () ->
        c.Channels.really_write data >>= fun () ->
        return Int64.(of_int (Cstruct.len data))
      | `Empty _n -> (* must be prezeroed *)
        assert prezeroed;
        return 0L
      | _ -> fail (Failure (Printf.sprintf "unexpected stream element: %s" (Vhd_format.Element.to_string x))) ) >>= fun work ->
    let sector = Int64.add sector (Vhd_format.Element.len x) in
    let work_done = Int64.add work_done work in
    p work_done;
    return (sector, work_done)
  ) (0L, 0L) s.elements >>= fun _ ->
  p total_work;

  (* Send the end-of-stream marker *)
  Chunked.marshal header { Chunked.offset = 0L; data = Cstruct.create 0 };
  c.Channels.really_write header >>= fun () ->

  return (Some total_work)

let stream_raw _common c s prezeroed _ ?(progress = no_progress_bar) () =
  (* Work to do is: non-zero data to write + empty sectors if the
     target is not prezeroed *)
  let total_work = let open Vhd_format.F in Int64.(add (add s.size.metadata s.size.copy) (if prezeroed then 0L else s.size.empty)) in
  let p = progress total_work in

  ( if not prezeroed then expand_empty s else return s ) >>= fun s ->

  fold_left (fun work_done x ->
    (match x with
      | `Copy(fd, sector_start, sector_len) ->
        let fd = Vhd_format_lwt.IO.to_file_descr fd in
        Lwt_unix.LargeFile.lseek fd (Int64.mul 512L sector_start) Unix.SEEK_SET
        >>= fun (_: int64) ->
        c.Channels.copy_from fd (Int64.mul 512L sector_len)
      | `Sectors data ->
        c.Channels.really_write data >>= fun () ->
        return Int64.(of_int (Cstruct.len data))
      | `Empty n -> (* must be prezeroed *)
        c.Channels.skip (Int64.(mul n 512L)) >>= fun () ->
        assert prezeroed;
        return 0L
    ) >>= fun work ->
    let work_done = Int64.add work_done work in
    p work_done;
    return work_done
  ) 0L s.elements >>= fun _ ->
  p total_work;

  return (Some total_work)

let sha1_update_cstruct ctx buffer =
  let ofs = buffer.Cstruct.off in
  let len = buffer.Cstruct.len in
  let buf = buffer.Cstruct.buffer in
  let buffer' : (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t = Bigarray.Array1.sub buf ofs len in
  (* XXX: need a better way to do this *)
  (* let buffer'': (int,  Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t = Obj.magic buffer' in *)
  Sha1.update_buffer ctx buffer'

module TarStream = struct
  type t = {
    work_done: int64;
    total_size: int64;
    ctx: Sha1.ctx;
    nr_bytes_remaining: int; (* start at 0 *)
    next_counter: int;
    mutable header: Tar.Header.t option;
  }

  let to_string t =
    Printf.sprintf "work_done = %Ld; nr_bytes_remaining = %d; next_counter = %d; filename = %s"
      t.work_done t.nr_bytes_remaining t.next_counter
      (match t.header with None -> "None" | Some h -> h.Tar.Header.file_name)

  let initial total_size = {
    work_done = 0L; ctx = Sha1.init (); nr_bytes_remaining = 0;
    next_counter = 0; header = None; total_size
  }

  let make_tar_header prefix counter suffix file_size =
    Tar.Header.make
      ~mod_time:(Int64.of_float (Unix.gettimeofday ()))
      ~file_mode:0o0644
      (Printf.sprintf "%s%08d%s" prefix counter suffix)
      (Int64.of_int file_size)
end

let stream_tar _common c s _ prefix ?(progress = no_progress_bar) () =
  let open TarStream in
  let prefix = match prefix with None -> "" | Some x -> x in
  let block_size = 1024 * 1024 in
  let header = IO.alloc Tar.Header.length in
  let zeroes = IO.alloc block_size in
  for i = 0 to Cstruct.len zeroes - 1 do
    Cstruct.set_uint8 zeroes i 0
  done;
  (* This undercounts by missing the tar headers and occasional empty sector *)
  let total_work = let open Vhd_format.F in Int64.(add s.size.metadata s.size.copy) in
  let p = progress total_work in

  expand_copy s >>= fun s ->

  (* Write [data] to the tar-format stream currnetly in [state] *)
  let rec input state data =
    (* Write as much as we can into the current file *)
    let len = Cstruct.len data in
    let this_block_len = min len state.nr_bytes_remaining in
    let this_block = Cstruct.sub data 0 this_block_len in
    sha1_update_cstruct state.ctx this_block;
    c.Channels.really_write this_block >>= fun () ->
    let nr_bytes_remaining = state.nr_bytes_remaining - this_block_len in
    let state = { state with nr_bytes_remaining } in
    let rest = Cstruct.shift data this_block_len in
    (* If we've hit the end of a block then output the hash *)
    ( if nr_bytes_remaining = 0 then match state.header with
      | Some hdr ->
        c.Channels.really_write (Tar.Header.zero_padding hdr) >>= fun () ->
        let hash = Sha1.(to_hex (finalize state.ctx)) in
        let ctx = Sha1.init () in
        let hdr' = { hdr with
          Tar.Header.file_name = hdr.Tar.Header.file_name ^ ".checksum";
          file_size = Int64.of_int (String.length hash)
        } in
        Tar.Header.marshal header hdr';
        c.Channels.really_write header >>= fun () ->
        Cstruct.blit_from_string hash 0 header 0 (String.length hash);
        c.Channels.really_write (Cstruct.sub header 0 (String.length hash)) >>= fun () ->
        c.Channels.really_write (Tar.Header.zero_padding hdr') >>= fun () ->
        return { state with ctx; header = None }
      | None ->
        return state
      else return state ) >>= fun state ->

    (* If we have unwritten data then output the next header *)
    ( if nr_bytes_remaining = 0 && Cstruct.len rest > 0 then begin
        (* XXX the last block might be smaller than block_size *)
        let hdr = make_tar_header prefix state.next_counter "" block_size in
        Tar.Header.marshal header hdr;
        c.Channels.really_write header >>= fun () ->
        return { state with nr_bytes_remaining = block_size;
                 next_counter = state.next_counter + 1;
                 header = Some hdr }
      end else return { state with nr_bytes_remaining } ) >>= fun state ->

    if Cstruct.len rest > 0
    then input state rest
    else return state in

  let rec empty state bytes =
    let write state bytes =
      let this = Int64.(to_int (min bytes (of_int (Cstruct.len zeroes)))) in
      input state (Cstruct.sub zeroes 0 this) >>= fun state ->
      empty state Int64.(sub bytes (of_int this)) in
    if bytes = 0L
    then return state
    (* If we're in the middle of a block, then complete it *)
    else if 0 < state.nr_bytes_remaining && state.nr_bytes_remaining < block_size
    then begin
      let this = min (Int64.of_int state.nr_bytes_remaining) bytes in
      write state this >>= fun state ->
      empty state (Int64.sub bytes this)
    (* If we're the first or last block then always include *)
    end else if state.work_done = 0L || Int64.(sub state.total_size state.work_done <= (of_int block_size))
    then write state bytes
    else if bytes >= (Int64.of_int block_size) then begin
      (* If n > block_size (in sectors) then we can omit empty blocks *)
      empty { state with next_counter = state.next_counter + 1 } Int64.(sub bytes (of_int block_size))
    end else write state bytes in
  let module E = Vhd_format.Element in
  fold_left (fun state x ->
    (match x with
      | `Sectors data ->
        input state data
      | `Empty n ->
        empty state (Int64.(mul n 512L))
      | _ -> fail (Failure (Printf.sprintf "unexpected stream element: %s" (Vhd_format.Element.to_string x))) ) >>= fun state ->
    let work = Int64.mul (E.len x) 512L in
    let work_done = Int64.add state.work_done work in
    p work_done;
    return { state with work_done }
  ) (initial s.size.Vhd_format.F.total) s.elements >>= fun _ ->
  p total_work;

  return (Some total_work)

module TarInput = struct
  type t = {
    ctx: Sha1.ctx;
    offset: int64;
    detected_block_size: int64 option;
    last_sequence_number: int;
  }
  let initial () = {
    ctx = Sha1.init ();
    offset = 0L;
    detected_block_size = None;
    last_sequence_number = -1;
  }
end

let startswith prefix x =
  let prefix_len = String.length prefix in
  let x_len = String.length x in
  x_len >= prefix_len && (String.sub x 0 prefix_len = prefix)

let endswith suffix x =
  let suffix_len = String.length suffix in
  let x_len = String.length x in
  x_len >= suffix_len && (String.sub x (x_len - suffix_len) suffix_len = suffix)

let serve_vhd_to_raw total_size c dest prezeroed progress _ _ =
  if not prezeroed then failwith "unimplemented: prezeroed";

  let p = ref None in

  let open Channel_In in
  let open Vhd_format.F in
  let rec loop block_size_sectors_shift last_block blocks_seen = function
    | End -> return ()
    | Cons (Fragment.Header h, tl) -> tl () >>= loop h.Header.block_size_sectors_shift last_block blocks_seen
    | Cons (Fragment.BAT x, tl) ->
      (* total_size = number of bits set in the BAT *)
      let total_size = BAT.fold (fun _ _ acc -> Int64.succ acc) x 0L in
      p := Some (progress total_size);
      tl () >>= loop block_size_sectors_shift last_block blocks_seen
    | Cons (Fragment.Block (offset, data), tl) ->
      Vhd_format_lwt.IO.really_write dest (Int64.shift_left offset sector_shift) data >>= fun () ->
      let this_block = Int64.(shift_right offset block_size_sectors_shift) in
      let blocks_seen = if last_block <> this_block then Int64.succ blocks_seen else blocks_seen in
      (match !p with Some p -> p blocks_seen | None -> ());
      tl () >>= loop block_size_sectors_shift this_block blocks_seen
    | Cons (_, tl) -> tl () >>= loop block_size_sectors_shift last_block blocks_seen in
  openstream (Some total_size) c >>= fun stream ->
  loop 0 (-1L) 0L stream

let serve_tar_to_raw total_size c dest prezeroed progress expected_prefix ignore_checksums =
  let twomib = 2 * 1024 * 1024 in
  let buffer = IO.alloc twomib in
  let header = IO.alloc 512 in

  if not prezeroed then failwith "unimplemented: prezeroed";

  let p = progress total_size in

  let open TarInput in
  let rec loop t =
    p t.offset;
    if t.offset = total_size
    then return ()
    else
      c.Channels.really_read header >>= fun () ->
      match Tar.Header.unmarshal header with
      | None -> fail (Failure "failed to unmarshal header")
      | Some hdr ->
        ( match expected_prefix with
          | None -> return (Filename.basename hdr.Tar.Header.file_name)
          | Some p ->
            if not(startswith p hdr.Tar.Header.file_name)
            then fail (Failure (Printf.sprintf "expected filename prefix %s, got %s" p hdr.Tar.Header.file_name))
            else
              let p_len = String.length p in
              let file_name_len = String.length hdr.Tar.Header.file_name in
              let filename = String.sub hdr.Tar.Header.file_name p_len (file_name_len - p_len) in
              return (Filename.basename filename)) >>= fun filename ->
        let zero = Cstruct.sub header 0 (Tar.Header.compute_zero_padding_length hdr) in
        (* either 'counter' or 'counter.checksum' *)
        if endswith ".checksum" filename then begin
          let checksum = Cstruct.sub buffer 0 (Int64.to_int hdr.Tar.Header.file_size) in
          c.Channels.really_read checksum >>= fun () ->
          c.Channels.really_read zero >>= fun () ->
          if ignore_checksums
          then loop t
          else begin
            let checksum' = Cstruct.to_string checksum in
            let hash = Sha1.(to_hex (finalize t.ctx)) in
            if checksum' <> hash then begin
              Printf.fprintf stderr "Unexpected checksum in %s: expected %s, we computed %s\n"
                hdr.Tar.Header.file_name checksum' hash;
              fail (Failure (Printf.sprintf "Unexpected checksum in block %s" hdr.Tar.Header.file_name))
            end else loop { t with ctx = Sha1.init () }
          end
        end else begin
          let block_size = match t.detected_block_size with
            | None -> hdr.Tar.Header.file_size
            | Some x -> x in
          ( try return (int_of_string filename)
            with _ -> fail (Failure (Printf.sprintf "Expected sequence number, got %s" filename)) ) >>= fun sequence_number ->
          let skipped_blocks = sequence_number - t.last_sequence_number - 1 in
          let to_skip = Int64.(mul (of_int skipped_blocks) block_size) in
          let offset = Int64.(add t.offset to_skip) in
          (* XXX: prezeroed? *)
          let rec copy offset remaining =
            let this = Int64.(to_int (min remaining (of_int (Cstruct.len buffer)))) in
            let block = Cstruct.sub buffer 0 this in
            c.Channels.really_read block >>= fun () ->
            Vhd_format_lwt.IO.really_write dest offset block >>= fun () ->
            if not ignore_checksums then sha1_update_cstruct t.ctx block;
            let remaining = Int64.(sub remaining (of_int this)) in
            let offset = Int64.(add offset (of_int this)) in
            if remaining = 0L
            then return offset
            else copy offset remaining in
          copy offset hdr.Tar.Header.file_size >>= fun offset ->
          c.Channels.really_read zero >>= fun () ->
          loop { t with offset; detected_block_size = Some block_size; last_sequence_number = sequence_number }
        end in
  loop (TarInput.initial ())

open StreamCommon

type endpoint =
  | Stdout
  | Null
  | File_descr of Lwt_unix.file_descr
  | Sockaddr of Lwt_unix.sockaddr
  | File of string
  | Http of Uri.t
  | Https of Uri.t

let endpoint_of_string = function
  | "stdout:" -> return Stdout
  | "null:" -> return Null
  | uri ->
    let uri' = Uri.of_string uri in
    begin match Uri.scheme uri', Uri.host uri' with
    | Some "fd", Some fd ->
      return (File_descr (fd |> int_of_string |> file_descr_of_int |> Lwt_unix.of_unix_file_descr))
    | Some "tcp", _ ->
      let host = match Uri.host uri' with None -> failwith "Please supply a host in the URI" | Some host -> host in
      let port = match Uri.port uri' with None -> failwith "Please supply a port in the URI" | Some port -> port in
      Lwt_unix.gethostbyname host >>= fun host_entry ->
      return (Sockaddr(Lwt_unix.ADDR_INET(host_entry.Lwt_unix.h_addr_list.(0), port)))
    | Some "unix", _ ->
      return (Sockaddr(Lwt_unix.ADDR_UNIX(Uri.path uri')))
    | Some "file", _ ->
      return (File(Uri.path uri'))
    | Some "http", _ ->
      return (Http uri')
    | Some "https", _ ->
      return (Https uri')
    | Some x, _ ->
      fail (Failure (Printf.sprintf "Unknown URI scheme: %s" x))
    | None, _ ->
      fail (Failure (Printf.sprintf "Failed to parse URI: %s" uri))
    end

let socket sockaddr =
  let family = match sockaddr with
  | Lwt_unix.ADDR_INET(_, _) -> Unix.PF_INET
  | Lwt_unix.ADDR_UNIX _ -> Unix.PF_UNIX in
  Lwt_unix.socket family Unix.SOCK_STREAM 0

let colon = Re.Str.regexp_string ":"

let retry common retries f =
  let rec aux n =
    if n <= 0 then f ()
    else
      Lwt.catch f
        (fun exn ->
           if common.Common.debug then
             Printf.fprintf stderr "warning: caught %s; will retry %d more time%s...\n%!"
               (Printexc.to_string exn) n (if n=1 then "" else "s");
           Lwt_unix.sleep 1. >>= fun () ->
           aux (n - 1)) in
  aux retries

(** [make_stream common source relative_to source_format destination_format]
    returns a lazy stream of extents to copy. [source_format] determines the
    way in which the [source] and [relative_to] strings sould be interpreted
    and how their data and metadata can be accessed. If [relative_to] is
    specified, then the changes from it will will be returned. *)
let make_stream common source relative_to source_format destination_format =
  match source_format, destination_format with
  | "nbdhybrid", "raw" ->
    begin match Re.Str.bounded_split colon source 4 with
    | [ raw; nbd_server; export_name; size ] -> begin
      let size = Int64.of_string size in
      Vhd_format_lwt.IO.openfile raw false >>= fun raw ->
      Nbd_input.raw raw nbd_server export_name size
      end
    | _ ->
      fail (Failure (Printf.sprintf "Failed to parse nbdhybrid source: %s (expecting <raw_disk>:<nbd_server>:<export_name>:<size>" source))
    end
  | "hybrid", "raw" ->
    (* expect source to be block_device:vhd *)
    begin match Re.Str.bounded_split colon source 2 with
    | [ raw; vhd ] ->
      let path = common.path @ [ Filename.dirname vhd ] in
      retry common 3 (fun () -> Vhd_IO.openchain ~path vhd false) >>= fun t ->
      Vhd_IO.close t >>= fun () ->
      Vhd_format_lwt.IO.openfile raw false >>= fun raw ->
      ( match relative_to with None -> return None | Some f -> Vhd_IO.openchain ~path f false >>= fun t -> Vhd_IO.close t >>= fun () -> return (Some t) ) >>= fun from ->
      Hybrid_input.raw ?from raw t
    | _ ->
      fail (Failure (Printf.sprintf "Failed to parse hybrid source: %s (expected raw_disk|vhd_disk)" source))
    end
  | "hybrid", "vhd" ->
    (* expect source to be block_device:vhd *)
    begin match Re.Str.bounded_split colon source 2 with
    | [ raw; vhd ] ->
      let path = common.path @ [ Filename.dirname vhd ] in
      retry common 3 (fun () -> Vhd_IO.openchain ~path vhd false) >>= fun t ->
      Vhd_IO.close t >>= fun () ->
      Vhd_format_lwt.IO.openfile raw false >>= fun raw ->
      ( match relative_to with None -> return None | Some f -> Vhd_IO.openchain ~path f false >>= fun t -> Vhd_IO.close t >>= fun () -> return (Some t) ) >>= fun from ->
      Hybrid_input.vhd ?from raw t
    | _ ->
      fail (Failure (Printf.sprintf "Failed to parse hybrid source: %s (expected raw_disk|vhd_disk)" source))
    end
  | "vhd", "vhd" ->
    let path = common.path @ [ Filename.dirname source ] in
    retry common 3 (fun () -> Vhd_IO.openchain ~path source false) >>= fun t ->
    ( match relative_to with None -> return None | Some f -> Vhd_IO.openchain ~path f false >>= fun t -> return (Some t) ) >>= fun from ->
    Vhd_input.vhd ?from t
  | "vhd", "raw" ->
    let path = common.path @ [ Filename.dirname source ] in
    retry common 3 (fun () -> Vhd_IO.openchain ~path source false) >>= fun t ->
    ( match relative_to with None -> return None | Some f -> Vhd_IO.openchain ~path f false >>= fun t -> return (Some t) ) >>= fun from ->
    Vhd_input.raw ?from t
  | "raw", "vhd" ->
    let source = match Image.of_device source with
      | Some (`Raw x) -> x (* bypass any tapdisk and use the raw file *)
      | _ -> source in
    Raw_IO.openfile source false >>= fun t ->
    Raw_input.vhd t
  | "raw", "raw" ->
    let source = match Image.of_device source with
      | Some (`Raw x) -> x (* bypass any tapdisk and use the raw file *)
      | _ -> source in
    Raw_IO.openfile source false >>= fun t ->
    Raw_input.raw t
  | _, _ -> assert false

let write_stream common s destination _source_protocol destination_protocol prezeroed progress tar_filename_prefix ssl_legacy good_ciphersuites legacy_ciphersuites =
  endpoint_of_string destination >>= fun endpoint ->
  let use_ssl = match endpoint with Https _ -> true | _ -> false in
  ( match endpoint with
    | File path ->
      Lwt_unix.openfile path [ Unix.O_RDWR; Unix.O_CREAT ] 0o0644 >>= fun fd ->
      Channels.of_seekable_fd fd >>= fun c ->
      return (c, [ NoProtocol; Human; Tar ])
    | Null ->
      Lwt_unix.openfile "/dev/null" [ Unix.O_RDWR ] 0o0 >>= fun fd ->
      Channels.of_raw_fd fd >>= fun c ->
      return (c, [ NoProtocol; Human; Tar ])
    | Stdout ->
      Channels.of_raw_fd Lwt_unix.stdout >>= fun c ->
      return (c, [ NoProtocol; Human; Tar ])
    | File_descr fd ->
      Channels.of_raw_fd fd >>= fun c ->
      return (c, [ Nbd; NoProtocol; Chunked; Human; Tar ])
    | Sockaddr sockaddr ->
      let sock = socket sockaddr in
      Lwt.catch (fun () ->
          Lwt_unix.connect sock sockaddr
        ) (fun e ->
          Lwt_unix.close sock >>= fun () -> Lwt.fail e
        )
      >>= fun () ->
      Channels.of_raw_fd sock >>= fun c ->
      return (c, [ Nbd; NoProtocol; Chunked; Human; Tar ])
    | Https uri'
    | Http uri' ->
      (* TODO: https is not currently implemented *)
      let port = match Uri.port uri' with None -> (if use_ssl then 443 else 80) | Some port -> port in
      let host = match Uri.host uri' with None -> failwith "Please supply a host in the URI" | Some host -> host in
      Lwt_unix.gethostbyname host >>= fun host_entry ->
      let sockaddr = Lwt_unix.ADDR_INET(host_entry.Lwt_unix.h_addr_list.(0), port) in
      let sock = socket sockaddr in
      Lwt.catch (fun () ->
          Lwt_unix.connect sock sockaddr
        ) (fun e ->
          Lwt_unix.close sock >>= fun () -> Lwt.fail e
        )
      >>= fun () ->

      let open Cohttp in
      ( if use_ssl then Channels.of_ssl_fd sock ssl_legacy good_ciphersuites legacy_ciphersuites else Channels.of_raw_fd sock ) >>= fun c ->

      let module Request = Request.Make(Cohttp_unbuffered_io) in
      let module Response = Response.Make(Cohttp_unbuffered_io) in
      let headers = Header.init () in
      let k, v = Cookie.Cookie_hdr.serialize [ "chunked", "true" ] in
      let headers = Header.add headers k v in
      let headers = match Uri.userinfo uri' with
        | None -> headers
        | Some x ->
          begin match Re.Str.bounded_split_delim (Re.Str.regexp_string ":") x 2 with
          | [ user; pass ] ->
            let b = Cohttp.Auth.string_of_credential (`Basic (user, pass)) in
            Header.add headers "authorization" b
          | _ ->
            Printf.fprintf stderr "I don't know how to handle authentication for this URI.\n Try scheme://user:password@host/path\n";
            exit 1
          end in
      let request = Cohttp.Request.make ~meth:`PUT ~version:`HTTP_1_1 ~headers uri' in
      Request.write (fun _ -> return ()) request c >>= fun () ->
      Response.read (Cohttp_unbuffered_io.make_input c) >>= fun r ->
      begin match r with
      | `Invalid x -> fail (Failure (Printf.sprintf "Invalid HTTP response: %s"
      x))
      | `Eof -> fail (Failure "EOF while parsing HTTP response")
      | `Ok x ->
        let code = Code.code_of_status (Cohttp.Response.status x) in
        if Code.is_success code then begin
          let advertises_nbd =
            let headers = Header.to_list (Cohttp.Response.headers x) in
            let headers = List.map (fun (x, y) -> String.lowercase_ascii x, String.lowercase_ascii y) headers in
            let te = "transfer-encoding" in
            List.mem_assoc te headers && (List.assoc te headers = "nbd") in
          if advertises_nbd
          then return(c, [ Nbd ])
          else return(c, [ Chunked; NoProtocol ])
        end else fail (Failure (Code.reason_phrase_of_code code))
      end
    ) >>= fun (c, possible_protocols) ->
    let destination_protocol = match destination_protocol with
      | Some x -> x
      | None ->
        let t = List.hd possible_protocols in
        Printf.fprintf stderr "Using protocol: %s\n%!" (string_of_protocol t);
        t in
    if not(List.mem destination_protocol possible_protocols)
    then fail(Failure(Printf.sprintf "this destination only supports protocols: [ %s ]" (String.concat "; " (List.map string_of_protocol possible_protocols))))
    else
      let start = Unix.gettimeofday () in
      (match destination_protocol with
          | Nbd -> stream_nbd
          | Human -> stream_human
          | Chunked -> stream_chunked
          | Tar -> stream_tar
          | NoProtocol -> stream_raw) common c s prezeroed tar_filename_prefix ~progress () >>= fun p ->
      c.Channels.close () >>= fun () ->
      match p with
      | Some p ->
        let time = Unix.gettimeofday () -. start in
        let physical_rate = Int64.(to_float p /. time) in
        if common.Common.verb then begin
          let add_unit x =
            let kib = 1024. in
            let mib = kib *. 1024. in
            let gib = mib *. 1024. in
            let tib = gib *. 1024. in
            if x /. tib > 1. then Printf.sprintf "%.1f TiB" (x /. tib)
            else if x /. gib > 1. then Printf.sprintf "%.1f GiB" (x /. gib)
            else if x /. mib > 1. then Printf.sprintf "%.1f MiB" (x /. mib)
            else if x /. kib > 1. then Printf.sprintf "%.1f KiB" (x /. kib)
            else Printf.sprintf "%.1f B" x in

          Printf.printf "Time taken: %s\n" (hms (int_of_float time));
          Printf.printf "Physical data rate: %s/sec\n" (add_unit physical_rate);
          let open Vhd_format.F in
          let speedup = Int64.(to_float s.size.total /. (to_float p)) in
          Printf.printf "Speedup: %.1f\n" speedup;
          Printf.printf "Virtual data rate: %s/sec\n" (add_unit (physical_rate *. speedup));
        end;
        return ()
      | None -> return ()


let stream_t common args ?(progress = no_progress_bar) () =
  make_stream common args.StreamCommon.source args.StreamCommon.relative_to args.StreamCommon.source_format args.StreamCommon.destination_format >>= fun s ->
  write_stream common s args.StreamCommon.destination args.StreamCommon.source_protocol args.StreamCommon.destination_protocol args.StreamCommon.prezeroed progress args.StreamCommon.tar_filename_prefix args.StreamCommon.ssl_legacy args.StreamCommon.good_ciphersuites args.StreamCommon.legacy_ciphersuites

let stream common args =
  try
    Vhd_format_lwt.File.use_unbuffered := common.Common.unbuffered;

    let progress_bar = match args with
    | { StreamCommon.progress = true; machine = true ; _} -> machine_progress_bar
    | { StreamCommon.progress = true; machine = false ; _} -> console_progress_bar
    | _ -> no_progress_bar in

    let thread = stream_t common args ~progress:progress_bar () in
    Lwt_main.run thread;
    `Ok ()
  with Failure x ->
    `Error(true, x)

let serve_nbd_to_raw common size c dest _ _ _ _ =
  let flags = [] in
  let open Nbd.Protocol in
  let buf = Cstruct.create (Negotiate.sizeof `V1) in
  Negotiate.marshal buf (Negotiate.V1 { Negotiate.size; flags });
  c.Channels.really_write buf >>= fun () ->

  let twomib = 2 * 1024 * 1024 in
  let block = IO.alloc twomib in
  let inblocks fn request =
    let rec loop offset remaining =
      let n = min twomib remaining in
      let subblock = Cstruct.sub block 0 n in
      fn offset subblock >>= fun () ->
      let remaining = remaining - n in
      let offset = Int64.(add offset (of_int n)) in
      if remaining > 0 then loop offset remaining else return () in
    loop request.Request.from (Int32.to_int request.Request.len) in

  let req = Cstruct.create Request.sizeof in
  let rep = Cstruct.create Reply.sizeof in
  let rec serve_requests () =
    c.Channels.really_read req >>= fun () ->
    match Request.unmarshal req with
    | Error e -> fail e
    | Ok request ->
      if common.Common.debug
      then Printf.fprintf stderr "%s\n%!" (Request.to_string request);
      begin match request.Request.ty with
      | Command.Write ->
        inblocks (fun offset subblock ->
          c.Channels.really_read subblock >>= fun () ->
          Vhd_format_lwt.IO.really_write dest offset subblock
        ) request >>= fun () ->
        Reply.marshal rep { Reply.error = Ok (); handle = request.Request.handle };
        c.Channels.really_write rep
      | Command.Read ->
        Reply.marshal rep { Reply.error = Ok (); handle = request.Request.handle };
        c.Channels.really_write rep >>= fun () ->
        inblocks (fun offset subblock ->
          Vhd_format_lwt.IO.really_read dest offset subblock >>= fun () ->
          c.Channels.really_write subblock
        ) request
      | _ ->
        Reply.marshal rep { Reply.error = Error `EPERM; handle = request.Request.handle };
        c.Channels.really_write rep
      end >>= fun () ->
      serve_requests () in
  serve_requests ()

let serve_chunked_to_raw _ c dest _ _ _ _ =
  let header = Cstruct.create Chunked.sizeof in
  let twomib = 2 * 1024 * 1024 in
  let buffer = IO.alloc twomib in
  let rec loop () =
    c.Channels.really_read header >>= fun () ->
    if Chunked.is_last_chunk header then begin
      Printf.fprintf stderr "Received last chunk.\n%!";
      return ()
    end else begin
      let rec block offset remaining =
        let this = Int32.(to_int (min (of_int twomib) remaining)) in
        let buf = if this < twomib then Cstruct.sub buffer 0 this else buffer in
        c.Channels.really_read buf >>= fun () ->
        Vhd_format_lwt.IO.really_write dest offset buf >>= fun () ->
        let offset = Int64.(add offset (of_int this)) in
        let remaining = Int32.(sub remaining (of_int this)) in
        if remaining > 0l
        then block offset remaining
        else return () in
      block (Chunked.get_offset header) (Chunked.get_len header) >>= fun () ->
      loop ()
    end in
  loop ()

(* If we're using unbuffered IO, we write in whole sectors. We therefore might
  need to extend the cstruct to the next sector boundary *)
let round_up_to_sector unbuffered len =
  if unbuffered then
    let sector_size = 512 in
    (((len - 1) / sector_size) + 1) * sector_size
  else
    len

let serve_raw_to_raw common size c dest _ progress _ _ =
  let twomib = 2 * 1024 * 1024 in
  let buffer = IO.alloc twomib in
  let p = progress size in
  let rec loop offset remaining =
    let n = Int64.(to_int (min remaining (of_int (Cstruct.len buffer)))) in
    let rounded_n = round_up_to_sector common.unbuffered n in
    (* Create a buffer of the rounded-up size *)
    let block = Cstruct.sub buffer 0 rounded_n in
    begin
      if n <> rounded_n
      then Vhd_format_lwt.IO.really_read dest offset block
      else Lwt.return ()
    end >>= fun () ->
    (* Create a cstruct that's an alias to the above block,
       but only as long as the amount of data we're expecting *)
    let block2 = Cstruct.sub block 0 n in
    c.Channels.really_read block2 >>= fun () ->
    Vhd_format_lwt.IO.really_write dest offset block >>= fun () ->
    let offset = Int64.(add offset (of_int n)) in
    let remaining = Int64.(sub remaining (of_int n)) in begin
      p Int64.(sub size remaining);
      if remaining > 0L
      then loop offset remaining
      else return ()
    end in
  loop 0L size

let serve common_options source source_fd source_format source_protocol destination destination_fd destination_format destination_size prezeroed progress machine expected_prefix ignore_checksums =
  try
    Vhd_format_lwt.File.use_unbuffered := common_options.Common.unbuffered;

    let source_protocol = protocol_of_string (require "source-protocol" source_protocol) in

    let supported_formats = [ "raw"; "vhd" ] in
    if not (List.mem source_format supported_formats)
    then failwith (Printf.sprintf "%s is not a supported format" source_format);
    let supported_formats = [ "raw" ] in
    if not (List.mem destination_format supported_formats)
    then failwith (Printf.sprintf "%s is not a supported format" destination_format);
    let supported_protocols = [ NoProtocol; Chunked; Nbd; Tar ] in
    if not (List.mem source_protocol supported_protocols)
    then failwith (Printf.sprintf "%s is not a supported source protocol" (string_of_protocol source_protocol));

    let destination = match destination_fd with
    | None -> destination
    | Some fd -> "fd://" ^ (string_of_int fd) in

    let progress_bar = match progress, machine with
    | true, true -> machine_progress_bar
    | true, false -> console_progress_bar
    | _, _ -> no_progress_bar in

    let thread =
      endpoint_of_string destination >>= fun destination_endpoint ->
      ( match source_fd with
        | None -> endpoint_of_string source
        | Some fd -> return (File_descr (Lwt_unix.of_unix_file_descr (file_descr_of_int fd))) ) >>= fun source_endpoint ->
      ( match source_endpoint with
        | File_descr fd ->
          Channels.of_raw_fd fd >>= fun c ->
          return c
        | Sockaddr s ->
          let sock = socket s in
          Lwt_unix.bind sock s >>= fun () ->
          Lwt_unix.setsockopt sock Unix.SO_REUSEADDR true;
          Lwt_unix.listen sock 1;
          Lwt_unix.accept sock >>= fun (fd, _) ->
          Channels.of_raw_fd fd >>= fun c ->
          return c
        | File path ->
          let fd = Vhd_format_lwt.File.openfile path false 0 in
          Channels.of_raw_fd (Lwt_unix.of_unix_file_descr fd)
        | _ -> failwith (Printf.sprintf "Not implemented: serving from source %s" source) ) >>= fun source_sock ->
      ( match destination_endpoint with
        | File path ->
          ( if not(Sys.file_exists path) then begin
              Lwt_unix.openfile path [ Unix.O_CREAT; Unix.O_RDONLY ] 0o0644 >>= fun fd ->
              Lwt_unix.close fd
            end else return () ) >>= fun () ->
          Vhd_format_lwt.IO.openfile path true >>= fun fd ->
          let size = match destination_size with
            | None -> Vhd_format_lwt.File.get_file_size path
            | Some x -> x in
          if size=0L
          then fail (Failure "Non-zero size required (either a pre-existing destination file or specified via --destination-size on the command line)")
          else return (fd, size)
        | _ -> failwith (Printf.sprintf "Not implemented: writing to destination %s" destination) ) >>= fun (destination_fd, size) ->
      let fn = match source_format, source_protocol with
        | "raw", NoProtocol -> serve_raw_to_raw common_options size
        | "raw", Nbd        -> serve_nbd_to_raw     common_options size
        | "raw", Chunked    -> serve_chunked_to_raw common_options
        | "raw", Tar        -> serve_tar_to_raw size
        | "vhd", NoProtocol -> serve_vhd_to_raw size
        | _, _ -> failwith (Printf.sprintf "Not implemented: receiving format %s via protocol %s" source_format (StreamCommon.string_of_protocol source_protocol)) in
      fn source_sock destination_fd prezeroed progress_bar expected_prefix ignore_checksums >>= fun () ->
      let fd = Lwt_unix.unix_file_descr (Vhd_format_lwt.IO.to_file_descr destination_fd) in
      (try Vhd_format_lwt.File.fsync fd; return () with _ -> fail (Failure "fsync failed")) in
    Lwt_main.run thread;
    `Ok ()
  with Failure x ->
  `Error(false, x)
