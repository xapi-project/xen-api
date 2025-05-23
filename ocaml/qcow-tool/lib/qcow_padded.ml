(*
 * Copyright (C) 2017 Docker Inc
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

module Cstructs = Qcow_cstructs

module Make (B : Qcow_s.RESIZABLE_BLOCK) = struct
  include B

  let handle_error = function
    | `Disconnected ->
        Lwt.return (Error `Disconnected)
    | _ ->
        Format.kasprintf Lwt.fail_with "Unknown error in qcow_paddle.ml"

  let read base base_sector buf =
    let open Lwt in
    B.get_info base >>= fun base_info ->
    let buf_len = Int64.of_int (Cstructs.len buf) in
    let missing_sectors =
      Int64.sub
        Int64.(
          add base_sector
            (div buf_len (of_int base_info.Mirage_block.sector_size))
        )
        base_info.Mirage_block.size_sectors
    in
    if missing_sectors > 0L then (
      let available_sectors =
        Int64.(
          sub
            (div buf_len (of_int base_info.Mirage_block.sector_size))
            missing_sectors
        )
      in
      let bytes =
        Int64.(
          to_int
            (mul available_sectors (of_int base_info.Mirage_block.sector_size))
        )
      in
      let open Lwt.Infix in
      ( if bytes > 0 then
          B.read base base_sector (Cstructs.sub buf 0 bytes)
        else
          Lwt.return (Ok ())
      )
      >>= function
      | Error e ->
          handle_error e
      | Ok () ->
          Cstructs.(memset (shift buf (max 0 bytes)) 0) ;
          Lwt.return (Ok ())
    ) else
      B.read base base_sector buf >>= function
      | Error e ->
          handle_error e
      | Ok () ->
          Lwt.return (Ok ())
end
