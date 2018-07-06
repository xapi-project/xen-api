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

module Make_seekable(B: S) = struct
  include B

  let seek_mapped _ sector = Lwt.return (Ok sector)
  let seek_unmapped t _ =
    B.get_info t
    >>= fun info ->
    Lwt.return (Ok info.Mirage_block.size_sectors)
end

module Sparse_copy (From: SEEKABLE) (Dest: S) = struct

  module B = Mirage_block

  type error = [
    | `Different_sizes
    | `Is_read_only
    | `A of From.error | `B of Dest.write_error ]

  let pp_error ppf = function
    | `Different_sizes -> Fmt.string ppf "The blocks have different size"
    | `Is_read_only    -> Fmt.string ppf "The destination is read-only"
    | `A e             -> From.pp_error ppf e
    | `B e             -> Dest.pp_write_error ppf e

  let v ~src:(from: From.t) ~dst:(dest: Dest.t) =

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
    if total_size_from <> total_size_dest
    then Lwt.return (Error `Different_sizes)
    else begin

      (* We'll run multiple threads to try to overlap I/O *)
      let next_from_sector = ref 0L in
      let next_dest_sector = ref 0L in
      let failure = ref None in
      let m = Lwt_mutex.create () in

      let record_failure e =
        Lwt_mutex.with_lock m  (fun () -> match !failure with
            | Some _ -> Lwt.return ()
            | None   -> failure := Some e; Lwt.return ()
          ) in

      let thread () =
        (* A page-aligned 64KiB buffer *)
        let buffer = Io_page.(to_cstruct (get 8)) in
        let from_sectors = Cstruct.len buffer / from_info.B.sector_size in
        let dest_sectors = Cstruct.len buffer / dest_info.B.sector_size in
        let rec loop () =
          (* Grab a region of the disk to copy *)
          Lwt_mutex.with_lock m (fun () ->
              let next_from = !next_from_sector in
              let next_dest = !next_dest_sector in
              next_from_sector := Int64.(add next_from (of_int from_sectors));
              next_dest_sector := Int64.(add next_dest (of_int dest_sectors));
              Lwt.return (next_from, next_dest)
            ) >>= fun (next_from, next_dest) ->
          if next_from >= from_info.B.size_sectors
          then Lwt.return ()
          else begin
            (* Copy from [next_from, next_from + from_sectors], omitting
               unmapped subregions *)
            let rec inner x y =
              if x >= Int64.(add next_from (of_int from_sectors))
              || x >= from_info.B.size_sectors
              then loop ()
              else begin
                From.seek_mapped from x
                >>= function
                | Error e -> record_failure (`A e)
                | Ok x' ->
                  if x' > x
                  then inner x' Int64.(add y (sub x' x))
                  else begin
                    From.seek_unmapped from x
                    >>= function
                    | Error e ->
                      record_failure (`A e)
                    | Ok next_unmapped ->
                      (* Copy up to the unmapped block, or the end of
                         our chunk... *)
                      let copy_up_to = min next_unmapped Int64.(add next_from (of_int from_sectors)) in
                      let remaining = Int64.sub copy_up_to x in
                      let this_time = min (Int64.to_int remaining) from_sectors in
                      let buf = Cstruct.sub buffer 0 (from_info.B.sector_size * this_time) in
                      From.read from x [ buf ]
                      >>= function
                      | Error e -> record_failure (`A e)
                      | Ok () ->
                        Dest.write dest y [ buf ]
                        >>= function
                        | Error e ->
                          record_failure (`B e)
                        | Ok () ->
                          inner Int64.(add x (of_int this_time)) Int64.(add y (of_int this_time))
                  end
              end in
            inner next_from next_dest
          end in
        loop () in
      let threads = List.map thread [ (); (); (); (); (); (); (); () ] in
      Lwt.join threads
      >|= fun () ->
      match !failure with
      | None   -> Ok ()
      | Some e -> Error e
    end

end

module Copy (From: S) (Dest: S) = struct
  module From_seekable = Make_seekable(From)
  module Sparse_copy = Sparse_copy (From_seekable)(Dest)
  type error = Sparse_copy.error
  let pp_error = Sparse_copy.pp_error
  let v ~src ~dst = Sparse_copy.v ~src ~dst
end
