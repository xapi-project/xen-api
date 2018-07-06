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

module Compare (From: S) (Dest: S) = struct

  type error = [`A of From.error | `B of Dest.error ]

  let pp_error ppf = function
    | `A e -> From.pp_error ppf e
    | `B e -> Dest.pp_error ppf e

  let v (from: From.t) (dest: Dest.t) =

    From.get_info from
    >>= fun from_info ->
    Dest.get_info dest
    >>= fun dest_info ->

    let total_size_from =
      Int64.(mul from_info.B.size_sectors (of_int from_info.B.sector_size))
    in
    let total_size_dest =
      Int64.(mul dest_info.B.size_sectors (of_int dest_info.B.sector_size))
    in
    match compare
            (from_info.B.size_sectors, total_size_from)
            (dest_info.B.size_sectors, total_size_dest) with
    | ((-1) | 1) as x -> Lwt.return (Ok x)
    | _ ->

      let from_buffer = Io_page.(to_cstruct (get 8)) in
      let dest_buffer = Io_page.(to_cstruct (get 8)) in
      let sectors = Cstruct.len from_buffer / from_info.B.sector_size in

      let rec loop next =
        if next >= from_info.B.size_sectors
        then Lwt.return (Ok 0)
        else begin
          let remaining = Int64.sub from_info.B.size_sectors next in
          let this_time = min sectors (Int64.to_int remaining) in
          let from_buf =
            Cstruct.sub from_buffer 0 (from_info.B.sector_size * this_time)
          in
          let dest_buf =
            Cstruct.sub dest_buffer 0 (dest_info.B.sector_size * this_time)
          in
          From.read from next [ from_buf ]
          >>= function
          | Error e -> Lwt.return (Error (`A e))
          | Ok () ->
            Dest.read dest next [ dest_buf ]
            >>= function
            | Error e -> Lwt.return (Error (`B e))
            | Ok () ->
              match Cstruct.compare from_buf dest_buf with
              | 0 -> loop Int64.(add next (of_int this_time))
              | x -> Lwt.return (Ok x)
        end in
      loop 0L

end
