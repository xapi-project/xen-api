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
open Lwt_preemptive

type handle

external _init: Unix.file_descr -> Unix.file_descr -> handle = "stub_init"
external _cleanup: handle -> unit = "stub_cleanup"
external _direct_copy: handle -> int64 -> int64 = "stub_direct_copy"

let with_handle from_fd to_fd f =
  let unix_from_fd = Lwt_unix.unix_file_descr from_fd in
  let unix_to_fd = Lwt_unix.unix_file_descr to_fd in
  let handle = _init unix_from_fd unix_to_fd in
  Lwt.finalize
    (fun () -> f handle)
    (fun () ->
      _cleanup handle;
      Lwt.return_unit
    )

let _direct_copy handle _from_fd _to_fd len =
  (* Atlhough the FD is set to blocking mode (by [with_blocking_fd]),
   * the fcntl flags are only updated lazily, either by
   * first call to an Lwt_unix IO function or [wrap_syscall].
   * Perform `wrap_syscall` here to avoid EAGAIN *)
  detach (_direct_copy handle) len

let maybe_fdatasync stat to_fd =
  match stat.Lwt_unix.LargeFile.st_kind with
  | Unix.S_REG | Unix.S_BLK ->
    Lwt_unix.fdatasync to_fd
  | _ -> Lwt.return_unit

(* The OS implementation can return short (e.g. Linux will stop at a 2GiB boundary).
   This function keeps copying until all the bytes are copied. *)
let direct_copy from_fd to_fd len =
  (* direct_copy requires sockets in non-blocking mode *)
  let with_blocking_fd fd f =
    Lwt_unix.blocking fd
    >>= function
    | true -> f fd
    | false ->
      Lwt_unix.set_blocking fd true;
      (* [set_blocking] sets the flags lazily,
       * force them to be set by querying it *)
      Lwt_unix.blocking fd >>= function
      | false -> Lwt.fail_with "Failed to set FD to blocking mode"
      | true ->
      Lwt.catch
        (fun () ->
          f fd
          >>= fun r ->
          Lwt_unix.set_blocking fd false;
          return r
        ) (fun e ->
          Lwt_unix.set_blocking fd false;
          fail e) in

  let sync_limit = Int64.(mul 4L (mul 1024L 1024L)) in

  let write handle from_fd to_fd to_write =
    let rec loop remaining =
      if remaining > 0L then begin
        _direct_copy handle from_fd to_fd remaining
        >>= fun written ->
        loop (Int64.sub remaining written)
      end else return () in
    loop to_write >>= fun () ->
    return to_write
  in

  let min x y =
    if Int64.compare x y = -1 then x else y
  in

  with_blocking_fd from_fd
    (fun from_fd ->
      with_blocking_fd to_fd
        (fun to_fd ->
          with_handle from_fd to_fd
            (fun handle ->
              Lwt_unix.LargeFile.fstat to_fd
              >>= fun stat ->
              let rec loop remaining =
                if remaining > 0L then begin
                  let to_write = min sync_limit remaining in
                  write handle from_fd to_fd to_write
                  >>= fun written ->
                  maybe_fdatasync stat to_fd
                  >>= fun () ->
                  loop (Int64.sub remaining written)
                end else return () in
              loop len
            )
        )
    )

type t = {
  really_read: Cstruct.t -> unit Lwt.t;
  really_write: Cstruct.t -> unit Lwt.t;
  offset: int64 ref;
  skip: int64 -> unit Lwt.t;
  copy_from: Lwt_unix.file_descr -> int64 -> int64 Lwt.t;
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
  let copy_from from_fd len =
    direct_copy from_fd fd len
    >>= fun () ->
    offset := Int64.(add !offset len);
    return len in
  let close () = Lwt_unix.close fd in
  return { really_read; really_write; offset; skip; copy_from; close }

let of_seekable_fd fd =
  of_raw_fd fd >>= fun c ->
  let skip n =
    Lwt_unix.LargeFile.lseek fd n Unix.SEEK_CUR >>= fun offset ->
    c.offset := offset;
    return () in
  return { c with skip }

let _ =
  Ssl.init ()

let legacy_sslctx good_ciphersuites legacy_ciphersuites =
  let ctx = Ssl.create_context Ssl.SSLv23 Ssl.Client_context in
  Ssl.set_cipher_list ctx (good_ciphersuites ^ (match legacy_ciphersuites with "" -> "" | s -> (":" ^ s)));
  Ssl.disable_protocols ctx [Ssl.SSLv3];
  ctx

let good_sslctx good_ciphersuites =
  let ctx = Ssl.create_context Ssl.TLSv1_2 Ssl.Client_context in
  Ssl.set_cipher_list ctx good_ciphersuites;
  ctx

let of_ssl_fd fd ssl_legacy good_ciphersuites legacy_ciphersuites =
  let good_ciphersuites = match good_ciphersuites with None -> failwith "good_ciphersuites not specified" | Some x -> x in
  let legacy_ciphersuites = match legacy_ciphersuites with None -> "" | Some x -> x in
  let sslctx = if ssl_legacy then legacy_sslctx good_ciphersuites legacy_ciphersuites else good_sslctx good_ciphersuites in
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
  let copy_from from_fd len =
    direct_copy from_fd fd len
    >>= fun () ->
    offset := Int64.(add !offset len);
    return len in

  let close () =
    Lwt_ssl.close sock in
  return { really_read; really_write; offset; skip; copy_from; close }


