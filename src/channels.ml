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

open Lwt

type t = {
  really_read: Cstruct.t -> unit Lwt.t;
  really_write: Cstruct.t -> unit Lwt.t;
  offset: int64 ref;
  skip: int64 -> unit Lwt.t;
  close: unit -> unit Lwt.t
}

exception Impossible_to_seek

let of_raw_fd fd =
  let offset = ref 0L in
  let really_read buf =
    IO.complete "read" (Some !offset) Lwt_bytes.read fd buf >>= fun () ->
    offset := Int64.(add !offset (of_int (Cstruct.len buf)));
    return () in
  let really_write buf =
    IO.complete "write" (Some !offset) Lwt_bytes.write fd buf >>= fun () ->
    offset := Int64.(add !offset (of_int (Cstruct.len buf)));
    return () in
  let skip _ = fail Impossible_to_seek in
  let close () = Lwt_unix.close fd in
  return { really_read; really_write; offset; skip; close }

let of_seekable_fd fd =
  of_raw_fd fd >>= fun c ->
  let skip n =
    Lwt_unix.LargeFile.lseek fd n Unix.SEEK_CUR >>= fun offset ->
    c.offset := offset;
    return () in
  return { c with skip }

let sslctx =
  Ssl.init ();
  Ssl.create_context Ssl.SSLv23 Ssl.Client_context

let of_ssl_fd fd =
  Lwt_ssl.ssl_connect fd sslctx >>= fun sock ->
  let offset = ref 0L in
  let really_read buf =
    IO.complete "read" (Some !offset) Lwt_ssl.read_bytes sock buf >>= fun () ->
    offset := Int64.(add !offset (of_int (Cstruct.len buf)));
    return () in
  let really_write buf =
    IO.complete "write" (Some !offset) Lwt_ssl.write_bytes sock buf >>= fun () ->
    offset := Int64.(add !offset (of_int (Cstruct.len buf)));
    return () in
  let skip _ = fail Impossible_to_seek in
  let close () =
    Lwt_ssl.close sock in
  return { really_read; really_write; offset; skip; close }


