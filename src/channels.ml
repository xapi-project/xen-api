(*
 * Copyright (c) 2012 Citrix Inc
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

open Vhd_lwt
open Lwt

type t = {
  really_read: Cstruct.t -> unit Lwt.t;
  really_write: Cstruct.t -> unit Lwt.t;
  really_write_offset: int64 ref;
  skip: int64 -> unit Lwt.t;
  close: unit -> unit Lwt.t
}

exception Impossible_to_seek

let of_raw_fd fd =
  let really_write_offset = ref 0L in
  let really_read = complete "read" None Lwt_bytes.read fd in
  let really_write buf =
    complete "write" (Some !really_write_offset) Lwt_bytes.write fd buf >>= fun () ->
    really_write_offset := Int64.(add !really_write_offset (of_int (Cstruct.len buf)));
    return () in
  let skip _ = fail Impossible_to_seek in
  let close () = Lwt_unix.close fd in
  return { really_read; really_write; really_write_offset; skip; close }

let of_seekable_fd fd =
  of_raw_fd fd >>= fun c ->
  let skip n =
    Lwt_unix.LargeFile.lseek fd n Unix.SEEK_CUR >>= fun offset ->
    c.really_write_offset := offset;
    return () in
  return { c with skip }

let sslctx =
  Ssl.init ();
  Ssl.create_context Ssl.SSLv23 Ssl.Client_context

let of_ssl_fd fd =
  Lwt_ssl.ssl_connect fd sslctx >>= fun sock ->
  let really_write_offset = ref 0L in
  let really_read = complete "read" None Lwt_ssl.read_bytes sock  in
  let really_write buf =
    complete "write" (Some !really_write_offset) Lwt_ssl.write_bytes sock buf >>= fun () ->
    really_write_offset := Int64.(add !really_write_offset (of_int (Cstruct.len buf)));
    return () in
  let skip _ = fail Impossible_to_seek in
  let close () =
    Lwt_ssl.close sock in
  return { really_read; really_write; really_write_offset; skip; close }


