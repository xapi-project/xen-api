(*
 * Copyright (C) 2015 David Scott <dave@recoil.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)
open Result
open Sexplib.Std
open Qcow

let expect_ok = function
  | Ok x -> x
  | Error (`Msg m) -> failwith m

let (>>*=) m f =
  let open Lwt in
  m >>= function
  | Error x -> Lwt.return (Error x)
  | Ok x -> f x

let src =
  let src = Logs.Src.create "qcow" ~doc:"qcow2-formatted BLOCK device" in
  Logs.Src.set_level src (Some Logs.Info);
  src

module Log = (val Logs.src_log src : Logs.LOG)

module ReadWriteBlock = struct
  include Block
  let original_connect = connect
  let connect path = connect ~lock:true path
end

module Time = struct
  type 'a io = 'a Lwt.t
  let sleep_ns ns = Lwt_unix.sleep (Int64.to_float ns /. 1e9)
end

module TracedBlock = struct
  include ReadWriteBlock

  let length_of bufs = List.fold_left (+) 0 (List.map Cstruct.len bufs)

  let read t sector bufs =
    Log.info (fun f -> f "BLOCK.read %Ld len = %d" sector (length_of bufs));
    read t sector bufs

  let write t sector bufs =
    Log.info (fun f -> f "BLOCK.write %Ld len = %d" sector (length_of bufs));
    write t sector bufs

  let flush t =
    Log.info (fun f -> f "BLOCK.flush");
    flush t

  let resize t new_size =
    Log.info (fun f -> f "BLOCK.resize %Ld" new_size);
    resize t new_size

end

module type BLOCK = sig

  include Qcow_s.RESIZABLE_BLOCK

  val connect: string -> t Lwt.t
end

module UnsafeBlock = struct
  include ReadWriteBlock
  let flush _ = Lwt.return (Ok ())
end

module ReadOnlyBlock = struct
  include UnsafeBlock
  let connect path = original_connect ~lock:false path
  let write _ _ _ =
    failwith "write to a read-only virtual device"
  let resize _ _ =
    failwith "attempt to resize a read-only virtual device"
end

let handle_common common_options_t =
  if common_options_t.Common.debug then begin
    List.iter
      (fun src ->
        if Logs.Src.name src = "qcow"
        then Logs.Src.set_level src (Some Logs.Debug)
      ) (Logs.Src.list ())
  end

let spinner = [| '-'; '\\'; '|'; '/' |]
let spinner_idx = ref 0
let progress_bar_width = 70
let last_percent = ref (-1)
let last_spinner_time = ref (Unix.gettimeofday ())
let progress_cb ~percent =
  let now = Unix.gettimeofday () in
  if now -. (!last_spinner_time) > 0.5 || !last_percent <> percent then begin
    last_percent := percent;
    last_spinner_time := now;
    let line = Bytes.make (progress_bar_width + 8) '\000' in

    let len = (progress_bar_width * percent) / 100 in
    for i = 0 to len - 1 do
      Bytes.set line (4 + i) (if i = len - 1 then '>' else '#')
    done;
    Bytes.set line 0 '[';
    Bytes.set line 1 spinner.(!spinner_idx);
    Bytes.set line 2 ']';
    Bytes.set line 3 ' ';
    spinner_idx := (!spinner_idx + 1) mod (Array.length spinner);
    let percent' = Printf.sprintf "%3d%%" percent in
    String.blit percent' 0 line (progress_bar_width + 4) 4;
    Printf.printf "\r%s%!" (Bytes.to_string line);
    if percent = 100 then Printf.printf "\n"
  end

let progress_cb_json fd =
  let oc = Unix.out_channel_of_descr fd in
  let last_percent = ref (-1) in
  fun ~percent ->
    if !last_percent <> percent then begin
      last_percent := percent;
      output_string oc (Printf.sprintf "{ \"progress\": %d }\n" percent);
      flush_all ()
    end

let mib = Int64.mul 1024L 1024L

let info filename filter =
  let t =
    let open Lwt in
    Lwt_unix.openfile filename [ Lwt_unix.O_RDONLY ] 0
    >>= fun fd ->
    let buffer = Cstruct.create 1024 in
    Lwt_cstruct.complete (Lwt_cstruct.read fd) buffer
    >>= fun () ->
    let h, _ = expect_ok (Header.read buffer) in
    let original_sexp = Header.sexp_of_t h in
    let sexp = match filter with
      | None -> original_sexp
      | Some str -> Sexplib.Path.get ~str original_sexp in
    Printf.printf "%s\n" (Sexplib.Sexp.to_string_hum sexp);
    return (`Ok ()) in
  Lwt_main.run t

let write filename sector data trace =
  let block =
     if trace
     then (module TracedBlock: BLOCK)
     else (module ReadWriteBlock: BLOCK) in
  let module BLOCK = (val block: BLOCK) in
  let module B = Qcow.Make(BLOCK)(Time) in
  let t =
    let open Lwt in
    BLOCK.connect filename
    >>= fun x ->
    B.connect x
    >>= fun x ->
    let npages = (String.length data + 4095) / 4096 in
    let buf = Io_page.(to_cstruct (get npages)) in
    Cstruct.memset buf 0;
    Cstruct.blit_from_string data 0 buf 0 (String.length data);
    B.write x sector [ buf ]
    >>= function
    | Error _ -> failwith "write failed"
    | Ok () -> return (`Ok ()) in
  Lwt_main.run t

let read filename sector length trace =
  let block =
     if trace
     then (module TracedBlock: BLOCK)
     else (module ReadWriteBlock: BLOCK) in
  let module BLOCK = (val block: BLOCK) in
  let module B = Qcow.Make(BLOCK)(Time) in
  let t =
    let open Lwt in
    BLOCK.connect filename
    >>= fun x ->
    B.connect x
    >>= fun x ->
    let length = Int64.to_int length * 512 in
    let npages = (length + 4095) / 4096 in
    let buf = Io_page.(to_cstruct (get npages)) in
    B.read x sector [ buf ]
    >>= function
    | Error _ -> failwith "write failed"
    | Ok () ->
      let result = Cstruct.sub buf 0 length in
      Printf.printf "%s%!" (Cstruct.to_string result);
      return (`Ok ()) in
  Lwt_main.run t

let check filename =
  let module B = Qcow.Make(ReadOnlyBlock)(Time) in
  let open Lwt in
  let t =
  let rec retry = function
    | 0 ->
      Printf.fprintf stderr "Warning: file is being concurrently modified\n";
      Printf.fprintf stderr "We found no concrete problems, but the file is being modified too\n";
      Printf.fprintf stderr "quickly for us to read a consistent view.\n";
      return (`Ok ())
    | n ->
      ReadOnlyBlock.connect filename
      >>= fun block ->
      B.check block
      >>= function
      | Error (`Reference_outside_file(src, dst)) ->
        begin
          ReadOnlyBlock.disconnect block
          >>= fun () ->
          ReadOnlyBlock.connect filename
          >>= fun block ->
          ReadOnlyBlock.get_info block
          >>= fun info ->
          let size = Int64.(mul info.Mirage_block.size_sectors (of_int info.Mirage_block.sector_size)) in
          if dst > size then begin
            Printf.fprintf stderr "Error: detected a reference outside the file, from %Ld to %Ld while the file size is %Ld\n%!" src dst size;
            exit 1
          end else begin
            (* The file has grown, try again *)
            ReadOnlyBlock.disconnect block
            >>= fun () ->
            retry (n - 1)
          end
        end
      | Error _ -> failwith (Printf.sprintf "Qcow consistency check failed on %s" filename)
      | Ok x ->
        Printf.printf "Qcow file seems intact.\n";
        Printf.printf "Total free blocks: %Ld\n" x.B.free;
        Printf.printf "Total used blocks: %Ld\n" x.B.used;
        return (`Ok ()) in
        retry 5 in
  Lwt_main.run t

exception Non_zero

(* slow but performance is not a concern *)
let is_zero buffer =
  try
    for i = 0 to Cstruct.len buffer - 1 do
      if Cstruct.get_uint8 buffer i <> 0 then raise Non_zero
    done;
    true
  with Non_zero -> false

let handle_error pp_error =
  function
  | Error e ->
    let msg = Format.asprintf "%a" pp_error e in
    Lwt.return (`Error (false, msg))
  | Ok x -> Lwt.return (`Ok x)

let discard unsafe_buffering filename =
  let block =
     if unsafe_buffering
     then (module UnsafeBlock: BLOCK)
     else (module ReadWriteBlock: BLOCK) in
  let module BLOCK = (val block: BLOCK) in
  let module B = Qcow.Make(BLOCK)(Time) in
  let open Lwt in
  let t =
    BLOCK.connect filename
    >>= fun x ->
    BLOCK.get_info x
    >>= fun info ->
    B.connect x
    >>= fun x ->
    let module F = Mirage_block_combinators.Fast_fold(B) in
    F.mapped_s
      ~f:(fun acc sector buffer ->
        if is_zero buffer then begin
          let len = Cstruct.len buffer in
          assert (len mod info.Mirage_block.sector_size = 0);
          let n = Int64.of_int @@ len / info.Mirage_block.sector_size in
          if Int64.add sector n = info.Mirage_block.size_sectors then begin
            (* The last block in the file: this is our last chance to discard *)
            let sector = match acc with None -> sector | Some x -> x in
            let n = Int64.sub info.Mirage_block.size_sectors sector in
            let open Lwt.Infix in
            B.discard x ~sector ~n ()
            >>= function
            | Error _ -> Lwt.fail_with "error discarding block"
            | Ok () -> Lwt.return None
          end else begin
            (* start/extend the current zero region *)
            let acc = match acc with None -> Some sector | Some x -> Some x in
            Lwt.return acc
          end
        end else begin
          match acc with
          | Some start ->
            (* we accumulated zeros: discard them now *)
            let n = Int64.sub sector start in
            let open Lwt.Infix in
            begin
              B.discard x ~sector:start ~n ()
              >>= function
              | Error _ -> Lwt.fail_with "error discarding block"
              | Ok () -> Lwt.return None
            end
          | None ->
            Lwt.return None
        end
      ) None x
    >>*= fun _ ->
    return (Ok ()) in
  Lwt_main.run (t >>= handle_error B.pp_error)

let compact common_options_t unsafe_buffering filename =
  handle_common common_options_t;
  let block =
     if unsafe_buffering
     then (module UnsafeBlock: BLOCK)
     else (module ReadWriteBlock: BLOCK) in
  let module BLOCK = (val block: BLOCK) in
  let module B = Qcow.Make(BLOCK)(Time) in
  let open Lwt in
  let progress_cb = match common_options_t.Common.progress, common_options_t.Common.progress_fd with
    | true, None -> Some progress_cb
    | _, Some fd -> Some (progress_cb_json (Unix_representations.file_descr_of_int fd))
    | _, _ -> None in

  (* workaround for https://github.com/mirage/mirage-block-unix/issues/59 *)
  Lwt_main.run begin
    let open Lwt.Infix in
    Lwt_unix.LargeFile.stat filename
    >>= fun stat ->
    let bytes = stat.Lwt_unix.LargeFile.st_size in
    let remainder = Int64.rem bytes 512L in
    let padding_required = if remainder = 0L then 0L else Int64.sub 512L remainder in
    Lwt_unix.openfile filename [ Lwt_unix.O_WRONLY; Lwt_unix.O_APPEND ] 0o0
    >>= fun fd ->
    let buf = Cstruct.create (Int64.to_int padding_required) in
    Cstruct.memset buf 0;
    Lwt_cstruct.complete (Lwt_cstruct.write fd) buf
    >>= fun () ->
    Lwt_unix.close fd
  end;
  let t =
    BLOCK.connect filename
    >>= fun x ->
    B.connect x
    >>= fun x ->
    B.get_info x
    >>= fun info ->
    B.compact x ?progress_cb ()
    >>*= fun report ->
    if report.B.old_size = report.B.new_size
    then Printf.printf "I couldn't make the file any smaller. Consider running `discard`.\n"
    else begin
      let smaller_sectors = Int64.sub report.B.old_size report.B.new_size in
      let sector_size = Int64.of_int info.Mirage_block.sector_size in
      let smaller_mib = Int64.(div (mul smaller_sectors sector_size) mib) in
      Printf.printf "The file is now %Ld MiB smaller.\n" smaller_mib
    end;
    B.Debug.check_no_overlaps x
  in
  Lwt_main.run (t >>= handle_error B.pp_write_error)

let repair unsafe_buffering filename =
  let block =
     if unsafe_buffering
     then (module UnsafeBlock: BLOCK)
     else (module ReadWriteBlock: BLOCK) in
  let module BLOCK = (val block: BLOCK) in
  let module B = Qcow.Make(BLOCK)(Time) in
  let open Lwt in
  let t =
    BLOCK.connect filename
    >>= fun x ->
    B.connect x
    >>= fun x ->
    B.rebuild_refcount_table x
    >>*= fun () ->
    B.Debug.check_no_overlaps x
  in
  Lwt_main.run (t >>= handle_error B.pp_write_error)

let sha _common_options_t filename =
  let module B = Qcow.Make(ReadOnlyBlock)(Time) in
  let open Lwt in
  let t =
    Block.connect filename
    >>= fun x ->
    let config = B.Config.create ~read_only:true () in
    B.connect ~config x
    >>= fun x ->
    B.get_info x
    >>= fun info ->
    let ctx = Sha1.init () in
    let update_cstruct c =
      let b' = c.Cstruct.buffer in
      if c.Cstruct.off = 0 && c.Cstruct.len = (Bigarray.Array1.dim b')
      then Sha1.update_buffer ctx b'
      else begin
        let c' = Cstruct.create (Cstruct.len c) in
        Cstruct.blit c 0 c' 0 (Cstruct.len c);
        let b' = c'.Cstruct.buffer in
        Sha1.update_buffer ctx b'
      end in
    let buf = Io_page.(to_cstruct @@ get 1024) in
    let buf_sectors = Int64.of_int (Cstruct.len buf / info.Mirage_block.sector_size) in
    let rec loop sector =
      let remaining = Int64.sub info.Mirage_block.size_sectors sector in
      if remaining = 0L then Lwt.return_unit else begin
        let n = min buf_sectors remaining in
        let buf = Cstruct.sub buf 0 (Int64.to_int n * info.Mirage_block.sector_size) in
        B.read x sector [ buf ]
        >>= function
        | Error _ -> Lwt.fail_with (Printf.sprintf "Failed to read sector %Ld" sector)
        | Ok () ->
          update_cstruct buf;
          loop (Int64.add sector n)
      end in
    loop 0L
    >>= fun () ->
    let digest = Sha1.finalize ctx in
    Printf.printf "%s\n" (Sha1.to_hex digest);
    return (`Ok ()) in
  Lwt_main.run t

let decode filename output =
  let module B = Qcow.Make(Block)(Time) in
  let open Lwt in
  let t =
    Block.connect filename
    >>= fun x ->
    B.connect x
    >>= fun x ->
    B.get_info x
    >>= fun info ->
    let total_size = Int64.(mul info.Mirage_block.size_sectors (of_int info.Mirage_block.sector_size)) in
    Lwt_unix.openfile output [ Lwt_unix.O_WRONLY; Lwt_unix.O_CREAT ] 0o0644
    >>= fun fd ->
    Lwt_unix.LargeFile.ftruncate fd total_size
    >>= fun () ->
    Lwt_unix.close fd
    >>= fun () ->
    Block.connect output
    >>= fun y ->
    let module C = Mirage_block_combinators.Copy(B)(Block) in
    C.v ~src:x ~dst:y
    >>= function
    | Error _ -> failwith "copy failed"
    | Ok () -> return (`Ok ()) in
  Lwt_main.run t

let encode filename output =
  let module B = Qcow.Make(ReadWriteBlock)(Time) in
  let open Lwt in
  let t =
    ReadWriteBlock.connect filename
    >>= fun raw_input ->
    ReadWriteBlock.get_info raw_input
    >>= fun raw_input_info ->
    let total_size = Int64.(mul raw_input_info.Mirage_block.size_sectors (of_int raw_input_info.Mirage_block.sector_size)) in
    Lwt_unix.openfile output [ Lwt_unix.O_WRONLY; Lwt_unix.O_CREAT ] 0o0644
    >>= fun fd ->
    Lwt_unix.close fd
    >>= fun () ->
    ReadWriteBlock.connect output
    >>= fun raw_output ->
    B.create raw_output ~size:total_size ()
    >>= function
    | Error _ -> failwith (Printf.sprintf "Failed to create qcow formatted data on %s" output)
    | Ok qcow_output ->
      let module C = Mirage_block_combinators.Copy(Block)(B) in
      C.v ~src:raw_input ~dst:qcow_output
      >>= function
      | Error _ -> failwith "copy failed"
      | Ok () -> return (`Ok ()) in
  Lwt_main.run t

let create size strict_refcounts trace filename =
  let block =
     if trace
     then (module TracedBlock: BLOCK)
     else (module ReadWriteBlock: BLOCK) in
  let module BLOCK = (val block: BLOCK) in
  let module B = Qcow.Make(BLOCK)(Time) in
  let open Lwt in
  let t =
    Lwt_unix.openfile filename [ Lwt_unix.O_CREAT ] 0o0644
    >>= fun fd ->
    Lwt_unix.close fd
    >>= fun () ->
    BLOCK.connect filename
    >>= fun x ->
    B.create x ~size ~lazy_refcounts:(not strict_refcounts) ()
    >>= function
    | Error _ -> failwith (Printf.sprintf "Failed to create qcow formatted data on %s" filename)
    | Ok _ -> return (`Ok ()) in
  Lwt_main.run t

let pattern common_options_t trace filename size number =
  let block =
     if trace
     then (module TracedBlock: BLOCK)
     else (module ReadWriteBlock: BLOCK) in
  let module Uncached = (val block: BLOCK) in
  let module BLOCK = Qcow_block_cache.Make(Uncached) in
  let module B = Qcow.Make(BLOCK)(Time) in
  let open Lwt in
  let progress_cb = if common_options_t.Common.progress then Some progress_cb else None in
  let t =
    Lwt_unix.openfile filename [ Lwt_unix.O_CREAT ] 0o0644
    >>= fun fd ->
    Lwt_unix.close fd
    >>= fun () ->
    Uncached.connect filename
    >>= fun uncached ->
    BLOCK.connect uncached
    >>= fun x ->
    let config = B.Config.create ~discard:true () in
    B.create x ~size ~lazy_refcounts:true ~config ()
    >>= function
    | Error _ -> failwith (Printf.sprintf "Failed to create qcow formatted data on %s" filename)
    | Ok qcow ->
      let h = B.header qcow in
      B.get_info qcow
      >>= fun info ->
      let sector_size = info.Mirage_block.sector_size in
      let cluster_bits = Int32.to_int h.Qcow.Header.cluster_bits in
      let cluster_size = 1 lsl cluster_bits in
      let cluster_size_sectors = cluster_size / sector_size in
      begin match number with
      | 1 ->
        (* write to every other cluster: this should be worst case for the
           interval tree structure. *)
        let page = Io_page.(to_cstruct @@ get 1) in
        let buf = Cstruct.sub page 0 sector_size in
        let rec loop sector =
          if sector >= info.Mirage_block.size_sectors then Lwt.return_unit else begin
            let percent = Int64.(to_int (div (mul 100L sector) info.Mirage_block.size_sectors)) in
            (match progress_cb with Some f -> f ~percent | None -> ());
            (* Mark each sector with the sector number *)
            Cstruct.BE.set_uint64 buf 0 sector;
            B.write qcow sector [ buf ]
            >>= function
            | Error _ -> Lwt.fail_with "qcow write error"
            | Ok () ->
              (* every other cluster *)
              loop Int64.(add sector (mul 2L (of_int cluster_size_sectors)))
          end in
        loop 0L
        >>= fun () ->
        BLOCK.disconnect x
        >>= fun () ->
        Lwt.return (`Ok ())
      | 2 ->
        (* write to every cluster, and then discard every other cluster: this
           should be worst case for the compactor *)
        let pages = Io_page.(to_cstruct @@ get 1024) in (* 4 MiB *)
        Cstruct.memset pages 0;
        let sectors = Cstruct.len pages / sector_size in
        let rec loop sector =
          if sector >= info.Mirage_block.size_sectors then Lwt.return_unit else begin
            let percent = Int64.(to_int (div (mul 50L sector) info.Mirage_block.size_sectors)) in
            (match progress_cb with Some f -> f ~percent | None -> ());
            let to_write = min sectors Int64.(to_int (sub info.Mirage_block.size_sectors sector)) in
            let buf = Cstruct.sub pages 0 (to_write * sector_size) in
            let rec watermark n =
              if n >= to_write then () else begin
                Cstruct.BE.set_uint64 buf (n * sector_size) Int64.(add sector (of_int n));
                watermark (n + 1)
              end in
            watermark to_write;
            B.write qcow sector [ buf ]
            >>= function
            | Error _ -> Lwt.fail_with "qcow write error"
            | Ok () ->
              loop Int64.(add sector (of_int to_write))
          end in
        loop 0L
        >>= fun () ->
        let rec loop sector =
          if sector >= info.Mirage_block.size_sectors then Lwt.return_unit else begin
            let percent = 50 + Int64.(to_int (div (mul 50L sector) info.Mirage_block.size_sectors)) in
            (match progress_cb with Some f -> f ~percent | None -> ());
            B.discard qcow ~sector ~n:(Int64.of_int cluster_size_sectors) ()
            >>= function
            | Error _ -> Lwt.fail_with "qcow discard error"
            | Ok () ->
              (* every other cluster *)
              loop Int64.(add sector (mul 2L (of_int cluster_size_sectors)))
          end in
        loop 0L
        >>= fun () ->
        BLOCK.disconnect x
        >>= fun () ->
        Lwt.return (`Ok ())
      | _ -> failwith (Printf.sprintf "Unknown pattern %d" number)
      end in
  Lwt_main.run t

let resize trace filename new_size ignore_data_loss =
  let block =
     if trace
     then (module TracedBlock: BLOCK)
     else (module ReadWriteBlock: BLOCK) in
  let module BLOCK = (val block: BLOCK) in
  let module B = Qcow.Make(BLOCK)(Time) in
  let open Lwt in
  let t =
    BLOCK.connect filename
    >>= fun block ->
    B.connect block
    >>= fun qcow ->
    B.get_info qcow
    >>= fun info ->
    let data_loss =
      let existing_size = Int64.(mul info.Mirage_block.size_sectors (of_int info.Mirage_block.sector_size)) in
      existing_size > new_size in
    if not ignore_data_loss && data_loss
    then return (`Error(false, "Making a disk smaller results in data loss:\ndisk is currently %Ld bytes which is larger than requested %Ld\n.Please see the --ignore-data-loss option."))
    else begin
      B.resize qcow ~new_size ~ignore_data_loss ()
      >>= function
      | Error _ -> failwith (Printf.sprintf "Failed to resize qcow formatted data on %s" filename)
      | Ok _ -> return (`Ok ())
    end in
  Lwt_main.run t

type output = [
  | `Text
  | `Json
]

let is_zero buf =
  let rec loop ofs =
    (ofs >= Cstruct.len buf) || (Cstruct.get_uint8 buf ofs = 0 && (loop (ofs + 1))) in
  loop 0

let mapped filename _format ignore_zeroes =
  let module B = Qcow.Make(Block)(Time) in
  let open Lwt in
  let t =
    Block.connect filename
    >>= fun x ->
    B.connect x
    >>= fun x ->
    B.get_info x
    >>= fun info ->
    Printf.printf "# offset (bytes), length (bytes)\n";
    let module F = Mirage_block_combinators.Fast_fold(B) in
    F.mapped_s ~f:(fun () sector_ofs data ->
      let sector_bytes = Int64.(mul sector_ofs (of_int info.Mirage_block.sector_size)) in
      if not ignore_zeroes || not(is_zero data)
      then Printf.printf "%Lx %d\n" sector_bytes (Cstruct.len data);
      Lwt.return_unit
    ) () x
    >>*= fun () ->
    return (Ok ()) in
  Lwt_main.run (t >>= handle_error B.pp_error)

let finally f g =
  try
    let r = f () in
    g ();
    r
  with e ->
    g ();
    raise e

type metadata = {
  blocks: Qcow.Int64.IntervalSet.t;
  total_size: int64;
} [@@deriving sexp]

let dehydrate _common input_filename output_filename =
  let module B = Qcow.Make(ReadOnlyBlock)(Time) in
  let open Lwt.Infix in
  let t =

    (* NB: all resources are only freed when the CLI exits. Don't copy this
       code into a long-running program! *)

    (* Extract the set of metadata blocks *)
    Block.connect input_filename
    >>= fun x ->
    Block.get_info x
    >>= fun info ->
    let total_size = Int64.(mul info.Mirage_block.size_sectors (of_int info.Mirage_block.sector_size)) in

    let config = B.Config.create ~read_only:true () in
    B.connect ~config x
    >>= fun qcow ->
    let blocks = B.Debug.metadata_blocks qcow in
    (* Open input and output file descriptors *)
    let buffer = Cstruct.create 1048576 in
    Lwt_unix.openfile input_filename [ Unix.O_RDONLY ] 0
    >>= fun input_fd ->
    Lwt_unix.openfile (output_filename ^ ".meta") [ Unix.O_EXCL; Unix.O_CREAT; Unix.O_WRONLY ] 0o0644
    >>= fun output_fd ->
    (* Append the metadata intervals from the `input_fd` to `metadata_fd` *)
    Qcow.Int64.IntervalSet.fold_s
      (fun i () ->
        let x, y = Qcow.Int64.(IntervalSet.Interval.(x i, y i)) in
        let rec loop x =
          let remaining = Int64.(succ @@ sub y x) in
          if remaining = 0L then Lwt.return_unit else begin
            let this_time = min (Cstruct.len buffer) (Int64.to_int remaining) in
            let fragment = Cstruct.sub buffer 0 this_time in
            Lwt_unix.LargeFile.lseek input_fd x Lwt_unix.SEEK_SET
            >>= fun _ ->
            Lwt_cstruct.(complete (read input_fd) fragment)
            >>= fun () ->
            Lwt_cstruct.(complete (write output_fd) fragment)
            >>= fun () ->
            loop (Int64.add x remaining)
          end in
        loop x
      ) blocks ()
    >>= fun () ->
    let metadata = { blocks; total_size } in
    let sexp = sexp_of_metadata metadata in
    Sexplib.Sexp.save_hum ~perm:0o0644 (output_filename ^ ".map") sexp;
    Lwt.return (`Ok ()) in
  Lwt_main.run t

let rehydrate _common input_filename output_filename =
  let open Lwt.Infix in
  let t =

    (* NB: all resources are only freed when the CLI exits. Don't copy this
       code into a long-running program! *)

    let sexp = Sexplib.Sexp.load_sexp (input_filename ^ ".map") in
    let metadata = metadata_of_sexp sexp in
    (* Open input and output file descriptors *)
    let buffer = Cstruct.create 1048576 in
    Lwt_unix.openfile (input_filename ^ ".meta") [ Unix.O_RDONLY ] 0o0644
    >>= fun input_fd ->
    Lwt_unix.openfile output_filename [ Unix.O_EXCL; Unix.O_CREAT; Unix.O_WRONLY ] 0o0644
    >>= fun output_fd ->
    Lwt_unix.LargeFile.lseek output_fd (Int64.pred metadata.total_size) Lwt_unix.SEEK_SET
    >>= fun _ ->
    Lwt_unix.write output_fd (Bytes.of_string "\000") 0 1
    >>= fun _ ->
    (* Append the metadata intervals from the `input_fd` to `metadata_fd` *)
    Qcow.Int64.IntervalSet.fold_s
      (fun i () ->
        let x, y = Qcow.Int64.(IntervalSet.Interval.(x i, y i)) in
        let rec loop x =
          let remaining = Int64.(succ @@ sub y x) in
          if remaining = 0L then Lwt.return_unit else begin
            let this_time = min (Cstruct.len buffer) (Int64.to_int remaining) in
            let fragment = Cstruct.sub buffer 0 this_time in
            Lwt_unix.LargeFile.lseek output_fd x Lwt_unix.SEEK_SET
            >>= fun _ ->
            Lwt_cstruct.(complete (read input_fd) fragment)
            >>= fun () ->
            Lwt_cstruct.(complete (write output_fd) fragment)
            >>= fun () ->
            loop (Int64.add x remaining)
          end in
        loop x
      ) metadata.blocks ()
    >>= fun () ->
    Lwt.return (`Ok ()) in
  Lwt_main.run t
