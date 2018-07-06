(*
 * Copyright (C) 2015 David Scott <dave.scott@unikernel.com>
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
open Lwt.Infix
open Mirage_block_lwt_s
open Result
module B = Mirage_block

let (>>*=) x f = x >>= function | Ok q -> f q | Error e -> Lwt.return @@ Error e

module Fold (Block: S) = struct

  let s ~f init (b: Block.t) =
    Block.get_info b
    >>= fun info ->
    let buffer = Io_page.(to_cstruct (get 8)) in
    let sectors = Cstruct.len buffer / info.B.sector_size in

    let rec loop acc next =
      if next >= info.B.size_sectors
      then Lwt.return (Ok acc)
      else begin
        let remaining = Int64.sub info.B.size_sectors next in
        let this_time = min sectors (Int64.to_int remaining) in
        let buf = Cstruct.sub buffer 0 (info.B.sector_size * this_time) in
        Block.read b next [ buf ]
        >>*= fun () ->
        f acc Int64.(mul next (of_int info.B.sector_size)) buf
        >>= fun acc ->
        loop acc Int64.(add next (of_int this_time))
      end in
    loop init 0L

end

module Fast_fold (Seekable: SEEKABLE) = struct

  let mapped_s ~f init (s: Seekable.t) =
    Seekable.get_info s
    >>= fun info ->
    let buffer = Io_page.(to_cstruct (get 8)) in
    let sectors = Cstruct.len buffer / info.B.sector_size in

    let rec loop acc next =
      (* next points to the next mapped chunk (or end of device) *)
      if next >= info.B.size_sectors
      then Lwt.return (Ok acc)
      else begin
        Seekable.seek_unmapped s next >>*= fun next_unmapped ->
        (* A chunk of data exists between next and next_unmapped *)
        let rec inner acc next =
          if next >= next_unmapped || next >= info.B.size_sectors
          then Lwt.return (Ok (acc, next))
          else begin
            let remaining = Int64.sub info.B.size_sectors next in
            let mapped = Int64.sub next_unmapped next in
            let this_time =
              min (min sectors (Int64.to_int remaining)) (Int64.to_int mapped)
            in
            let buf =
              Cstruct.sub buffer 0 (info.B.sector_size * this_time)
            in
            Seekable.read s next [ buf ] >>*= fun () ->
            f acc next buf >>= fun acc ->
            let next = Int64.(add next (of_int this_time)) in
            inner acc next
          end in
        inner acc next >>*= fun (acc, next) ->
        (* next points to the next unmapped chunk (or end of device) *)
        Seekable.seek_mapped s next
        >>*= fun next ->
        loop acc next
      end in
    Seekable.seek_mapped s 0L >>*= fun start -> loop init start

  let unmapped_s ~f init (s: Seekable.t) =
    Seekable.get_info s
    >>= fun info ->

    let rec loop acc next =
      (* next points to the next mapped chunk (or end of device) *)
      if next >= info.B.size_sectors
      then Lwt.return @@ Ok acc
      else begin
        Seekable.seek_unmapped s next
        >>*= fun next_unmapped ->
        Seekable.seek_mapped s next_unmapped
        >>*= fun next_mapped ->
        f acc next_unmapped (Int64.sub next_mapped next_unmapped)
        >>= fun acc ->
        loop acc next_mapped
      end in
    loop init 0L

end
