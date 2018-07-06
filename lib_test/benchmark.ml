(*
 * Copyright (C) 2017 Docker Inc
 *
 * Permission to use, copy, modify, and/or distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
 * REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
 * INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
 * LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
 * OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 *)

(* A simple file copy benchmark *)

let split_into buf into =
  let rec loop acc remaining =
    if Cstruct.len remaining = 0 then List.rev acc else begin
      let to_take = min into (Cstruct.len remaining) in
      loop (Cstruct.sub remaining 0 to_take :: acc) (Cstruct.shift remaining to_take)
    end in
  loop [] buf

let copy ~src ~dst ~bs ~split ~buffered ~threads =
  let open Lwt.Infix in
  Block.connect ~buffered src
  >>= fun src ->
  (* Create the destination if it doesn't exist *)
  Lwt_unix.openfile dst [ Lwt_unix.O_CREAT; Lwt_unix.O_TRUNC; Lwt_unix.O_WRONLY ] 0o0644
  >>= fun fd ->
  Lwt_unix.close fd
  >>= fun () ->
  Block.connect ~buffered dst
  >>= fun dst ->
  Block.get_info src
  >>= fun info ->
  Block.resize dst 0L
  >>= function
  | Error _ -> failwith (Printf.sprintf "Block.resize %Ld failed" 0L)
  | Ok () ->
  let m = Lwt_mutex.create () in
  let resize_to_atleast n =
    Lwt_mutex.with_lock m
      (fun () ->
        Block.get_info dst
        >>= fun dst_info ->
        if dst_info.Mirage_block.size_sectors < n then begin
          Block.resize dst n
          >>= function
          | Error _ -> failwith (Printf.sprintf "Block.resize %Ld failed" n)
          | Ok () -> Lwt.return_unit
        end else Lwt.return_unit
      ) in
  let bs_sectors = bs / info.Mirage_block.sector_size in
  let sector = ref 0L in
  let one_thread () =
    let bs_pages = (bs + 4093) / 4096 in
    let pages = Io_page.(to_cstruct @@ get bs_pages) in
    let rec loop () =
      let remaining = Int64.sub info.Mirage_block.size_sectors !sector in
      let this_time = min remaining (Int64.of_int bs_sectors) in
      if this_time = 0L then Lwt.return_unit else begin
        let start_sector = !sector in
        sector := Int64.add !sector this_time;
        let buf = Cstruct.sub pages 0 (Int64.to_int (this_time) * info.Mirage_block.sector_size) in
        let bufs = match split with
          | None -> [ buf ]
          | Some into -> split_into buf into in
        Block.read src start_sector bufs
        >>= function
        | Error _ -> failwith (Printf.sprintf "Block.read %Ld failed" start_sector)
        | Ok () ->
        resize_to_atleast (Int64.add start_sector this_time)
        >>= fun () ->
        Block.write dst start_sector bufs
        >>= function
        | Error _ -> failwith (Printf.sprintf "Block.write %Ld failed" start_sector)
        | Ok () ->
        loop ()
      end in
    loop () in
  let rec units = function
    | 0 -> []
    | n -> () :: (units (n - 1)) in
  let threads = List.map one_thread @@ units threads in
  Lwt.join threads
  >>= fun () ->

  Block.disconnect dst
  >>= fun () ->
  Block.disconnect src

let _ =
  let src = ref "benchmark.src" in
  let dst = ref "benchmark.dst" in
  let bs = ref 512 in
  let split = ref None in
  let buffered = ref true in
  let threads = ref 1 in
  Arg.parse [
    "-src", Arg.Set_string src, Printf.sprintf "source filename (default %s)" !src;
    "-dst", Arg.Set_string dst, Printf.sprintf "destination filename (default %s)" !dst;
    "-bs", Arg.Set_int bs, Printf.sprintf "block size (default %d)" !bs;
    "-split", Arg.Int (fun x -> split := Some x), "split each block into a list of fragments";
    "-buffered", Arg.Set buffered, Printf.sprintf "buffered (default %b)" !buffered;
    "-threads", Arg.Set_int threads, Printf.sprintf "threads (default %d)" !threads;
  ] (fun x ->
      Printf.fprintf stderr "Unrecognised argument: %s\nSee -help for usage.\n" x;
      exit 1;
    ) "A simple file copy benchmark";
  Logs.set_reporter (Logs_fmt.reporter ());
  Lwt_main.run @@ copy ~src:(!src) ~dst:(!dst) ~bs:(!bs) ~split:(!split) ~buffered:(!buffered) ~threads:(!threads)
