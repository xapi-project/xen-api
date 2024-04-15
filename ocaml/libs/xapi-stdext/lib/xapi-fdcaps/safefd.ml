(*
 * Copyright (C) 2023 Cloud Software Group
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

let string_of_file_kind =
  let open Unix in
  function
  | S_REG ->
      "regular file"
  | S_BLK ->
      "block device"
  | S_CHR ->
      "character device"
  | S_DIR ->
      "directory"
  | S_LNK ->
      "symlink"
  | S_FIFO ->
      "FIFO/pipe"
  | S_SOCK ->
      "socket"

let pp_kind = Fmt.of_to_string string_of_file_kind

module Identity = struct
  type t = {
      kind: Unix.file_kind
    ; device: int
    ; inode: int (* should be int64? *)
  }

  let of_fd fd =
    let open Unix.LargeFile in
    let stat = fstat fd in
    {kind= stat.st_kind; device= stat.st_dev; inode= stat.st_ino}

  let same a b = a.kind = b.kind && a.device = b.device && a.inode = b.inode

  let pp =
    Fmt.(
      record
        ~sep:Fmt.(any ", ")
        [
          field "kind" (fun t -> t.kind) pp_kind
        ; field "device" (fun t -> t.device) int
        ; field "inode" (fun t -> t.inode) int
        ]
    )
end

type t = {
    fd: (Unix.file_descr, Printexc.raw_backtrace) result Atomic.t
  ; opened_at: Printexc.raw_backtrace
  ; original: Identity.t
}

let pp ppf t =
  (* print only essential info that fits on a single line *)
  Fmt.pf ppf "@[FD %a: %a@]"
    (Fmt.result ~ok:Fmt.(any "open") ~error:Fmt.(any "closed"))
    (Atomic.get t.fd) Identity.pp t.original

let pp_closed ppf bt =
  let exception Closed_at in
  Fmt.exn_backtrace ppf (Closed_at, bt)

let pp_opened_at ppf bt =
  let exception Opened_at in
  Fmt.exn_backtrace ppf (Opened_at, bt)

let dump =
  Fmt.(
    Dump.(
      record
        [
          field "fd"
            (fun t -> Atomic.get t.fd)
            Fmt.Dump.(result ~ok:(any "opened") ~error:pp_closed)
        ; field "opened_at" (fun t -> t.opened_at) pp_opened_at
        ; field "original" (fun t -> t.original) Identity.pp
        ]
    )
  )

let location () =
  (* We could raise and immediately catch an exception but that will have a very short stacktrace,
     [get_callstack] is better.
  *)
  Printexc.get_callstack 1000

let nop =
  {
    fd= Atomic.make (Error (location ()))
  ; opened_at= Printexc.get_callstack 0
  ; original= Identity.of_fd Unix.stdin
  }

let check_exn ~caller t fd =
  let actual = Identity.of_fd fd in
  if not (Identity.same t.original actual) then (
    let msg =
      Format.asprintf "@[<h>File descriptor mismatch: %a <> %a@]" Identity.pp
        t.original Identity.pp actual
    in
    (* invalidate FD so nothing else uses it anymore, we know it points to elsewhere now *)
    Atomic.set t.fd (Error (location ())) ;
    (* raise backtrace with original open location *)
    Printexc.raise_with_backtrace
      Unix.(Unix_error (EBADF, caller, msg))
      t.opened_at
  )

let close_common_exn t =
  let closed = Error (location ()) in
  (* ensure noone else can access it, before we close it *)
  match Atomic.exchange t.fd closed with
  | Error _ as e ->
      (* put back the original backtrace *)
      Atomic.set t.fd e ; e
  | Ok fd ->
      check_exn ~caller:"close_common_exn" t fd ;
      Ok (Unix.close fd)

let close_exn t =
  match close_common_exn t with
  | Error bt ->
      let ebadf = Unix.(Unix_error (EBADF, "close_exn", "")) in
      (* raise with previous close's backtrace *)
      Printexc.raise_with_backtrace ebadf bt
  | Ok () ->
      ()

let idempotent_close_exn t =
  let (_ : _ result) = close_common_exn t in
  ()

let leak_count = Atomic.make 0

let leaked () = Atomic.get leak_count

let finalise t =
  match Atomic.get t.fd with
  | Ok _ ->
      Atomic.incr leak_count ;
      if Sys.runtime_warnings_enabled () then
        Format.eprintf "@.Warning: leaked file descriptor detected:@,%a@]@."
          pp_opened_at t.opened_at
  | Error _ ->
      ()

let of_file_descr fd =
  let v =
    {
      fd= Atomic.make (Ok fd)
    ; opened_at= location ()
    ; original= Identity.of_fd fd
    }
  in
  Gc.finalise finalise v ; v

let unsafe_to_file_descr_exn t =
  match Atomic.get t.fd with
  | Ok fd ->
      fd
  | Error bt ->
      let ebadf = Unix.(Unix_error (EBADF, "unsafe_to_file_descr_exn", "")) in
      Printexc.raise_with_backtrace ebadf bt

let with_fd_exn t f =
  let fd = unsafe_to_file_descr_exn t in
  let r = f fd in
  check_exn ~caller:"with_fd_exn" t fd ;
  r

let setup () = Sys.set_signal Sys.sigpipe Sys.Signal_ignore
