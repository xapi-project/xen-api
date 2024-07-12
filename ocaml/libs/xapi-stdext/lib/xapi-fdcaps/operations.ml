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

open Properties

type +!'a props = {
    props: ('b, 'c) Properties.props
  ; custom_ftruncate: (int64 -> unit) option
  ; fd: Safefd.t
}
  constraint 'a = ('b, 'c) Properties.props

type +!'a t = 'a props constraint 'a = (_, _) Properties.t

type (+!'a, +!'b) make = ('a, 'b) Properties.t t

let dump ppf =
  Fmt.(
    Dump.(
      record
        [
          field "props" (fun t -> t.props) pp
        ; field "custom_ftruncate"
            (fun t -> Option.is_some t.custom_ftruncate)
            bool
        ; field "fd" (fun t -> t.fd) Safefd.dump
        ]
    )
  )
    ppf

let pp ppf =
  Fmt.(
    record
      ~sep:Fmt.(any "; ")
      [
        field "props" (fun t -> t.props) pp
      ; field "custom_ftruncate"
          (fun t -> Option.is_some t.custom_ftruncate)
          bool
      ; field "fd" (fun t -> t.fd) Safefd.pp
      ]
  )
    ppf

let close t = Safefd.idempotent_close_exn t.fd

let fsync t = Unix.fsync (Safefd.unsafe_to_file_descr_exn t.fd)

let as_readable_opt t =
  match as_readable_opt t.props with
  | None ->
      None
  | Some props ->
      Some {t with props}

let as_writable_opt t =
  match as_writable_opt t.props with
  | None ->
      None
  | Some props ->
      Some {t with props}

let as_spipe_opt t =
  match
    (Properties.as_kind_opt `sock t.props, Properties.as_kind_opt `fifo t.props)
  with
  | Some props, _ | _, Some props ->
      Some {t with props}
  | None, None ->
      None

let with_fd t f =
  let finally () = close t in
  Fun.protect ~finally (fun () -> f t)

module Syntax = struct let ( let@ ) f x = f x end

open Syntax

let with_fd2 (fd1, fd2) f =
  let@ fd1 = with_fd fd1 in
  let@ fd2 = with_fd fd2 in
  f (fd1, fd2)

let make ?custom_ftruncate props fd : 'a t =
  {fd= Safefd.of_file_descr fd; props; custom_ftruncate}

let make_ro_exn kind fd = make (Properties.make `rdonly kind) fd

let make_wo_exn kind fd = make (Properties.make `wronly kind) fd

let make_rw_exn ?custom_ftruncate kind fd =
  make (Properties.make `rdwr kind) ?custom_ftruncate fd

let pipe () =
  let kind = `fifo in
  let ro, wo = Unix.pipe ~cloexec:true () in
  (make_ro_exn kind ro, make_wo_exn kind wo)

let socketpair domain typ proto =
  let kind = `sock in
  let fd1, fd2 = Unix.socketpair ~cloexec:true domain typ proto in
  (make_rw_exn kind fd1, make_rw_exn kind fd2)

let openfile_ro kind path flags =
  make_ro_exn kind
  @@ Unix.openfile path (Unix.O_RDONLY :: Unix.O_CLOEXEC :: flags) 0

let openfile_rw ?custom_ftruncate kind path flags =
  make_rw_exn ?custom_ftruncate kind
  @@ Unix.openfile path (Unix.O_RDWR :: Unix.O_CLOEXEC :: flags) 0

let openfile_wo kind path flags =
  make_wo_exn kind
  @@ Unix.openfile path (Unix.O_WRONLY :: Unix.O_CLOEXEC :: flags) 0

let creat path flags perm =
  make_rw_exn `reg
  @@ Unix.openfile path
       (Unix.O_RDWR :: Unix.O_CREAT :: Unix.O_EXCL :: Unix.O_CLOEXEC :: flags)
       perm

let kind_of_fd fd = of_unix_kind Unix.LargeFile.((fstat fd).st_kind)

let stdin = make_ro_exn (kind_of_fd Unix.stdin) Unix.stdin

let stdout = make_wo_exn (kind_of_fd Unix.stdout) Unix.stdout

let stderr = make_wo_exn (kind_of_fd Unix.stderr) Unix.stderr

let dev_null_out () = openfile_wo `chr "/dev/null" []

let dev_null_in () = openfile_ro `chr "/dev/null" []

let dev_zero () = openfile_ro `chr "/dev/zero" []

let shutdown_recv t =
  Unix.shutdown (Safefd.unsafe_to_file_descr_exn t.fd) Unix.SHUTDOWN_RECEIVE

let shutdown_send t =
  Unix.shutdown (Safefd.unsafe_to_file_descr_exn t.fd) Unix.SHUTDOWN_SEND

let as_readonly_socket t =
  shutdown_send t ;
  {t with props= Properties.make `rdonly `sock}

let as_writeonly_socket t =
  shutdown_recv t ;
  {t with props= Properties.make `wronly `sock}

let shutdown_all t =
  Unix.shutdown (Safefd.unsafe_to_file_descr_exn t.fd) Unix.SHUTDOWN_ALL

let setsockopt_float t opt value =
  Unix.setsockopt_float (Safefd.unsafe_to_file_descr_exn t.fd) opt value

let ftruncate t size =
  match t.custom_ftruncate with
  | None ->
      Unix.LargeFile.ftruncate (Safefd.unsafe_to_file_descr_exn t.fd) size
  | Some f ->
      f size

let lseek t off whence =
  Unix.LargeFile.lseek (Safefd.unsafe_to_file_descr_exn t.fd) off whence

let read t buf off len =
  Unix.read (Safefd.unsafe_to_file_descr_exn t.fd) buf off len

let single_write_substring t buf off len =
  Unix.single_write_substring (Safefd.unsafe_to_file_descr_exn t.fd) buf off len

let fstat t = Unix.LargeFile.fstat (Safefd.unsafe_to_file_descr_exn t.fd)

let dup t =
  {
    t with
    fd=
      t.fd
      |> Safefd.unsafe_to_file_descr_exn
      |> Unix.dup
      |> Safefd.of_file_descr
  }

let set_nonblock t = Unix.set_nonblock (Safefd.unsafe_to_file_descr_exn t.fd)

let clear_nonblock t = Unix.clear_nonblock (Safefd.unsafe_to_file_descr_exn t.fd)

let with_tempfile ?size () f =
  let name, ch =
    Filename.open_temp_file ~mode:[Open_binary] "xapi_fdcaps" "tmp"
  in
  let finally () =
    close_out_noerr ch ;
    try Unix.unlink name with Unix.Unix_error (_, _, _) -> ()
  in
  let@ () = Fun.protect ~finally in
  let t = ch |> Unix.descr_of_out_channel |> make_wo_exn `reg in
  let@ t = with_fd t in
  size |> Option.iter (fun size -> ftruncate t size) ;
  f (name, t)

let check_output cmd args =
  let cmd = Filename.quote_command cmd args in
  let ch = Unix.open_process_in cmd in
  let finally () =
    try
      let (_ : Unix.process_status) = Unix.close_process_in ch in
      ()
    with _ -> ()
  in
  Fun.protect ~finally @@ fun () ->
  let out = In_channel.input_all ch |> String.trim in
  match Unix.close_process_in ch with
  | Unix.WEXITED 0 ->
      out
  | _ ->
      failwith (Printf.sprintf "%s exited nonzero" cmd)

let with_temp_blk ?(sector_size = 512) name f =
  let blkdev =
    check_output "losetup"
      [
        "--show"
      ; "--sector-size"
      ; string_of_int sector_size
      ; "--direct-io=on"
      ; "--find"
      ; name
      ]
  in
  let custom_ftruncate size =
    Unix.LargeFile.truncate name size ;
    let (_ : string) = check_output "losetup" ["--set-capacity"; name] in
    ()
  in
  let finally () =
    let (_ : string) = check_output "losetup" ["--detach"; blkdev] in
    ()
  in
  let@ () = Fun.protect ~finally in
  let@ t = with_fd @@ openfile_rw ~custom_ftruncate `blk blkdev [] in
  f (blkdev, t)

let setup () = Sys.set_signal Sys.sigpipe Sys.Signal_ignore

type ('a, 'b) operation = 'a t -> 'b -> int -> int -> int

let repeat_read op fd buf off len =
  let rec loop consumed =
    let off = off + consumed and len = len - consumed in
    if len = 0 then
      consumed (* we filled the buffer *)
    else
      match op fd buf off len with
      | 0 (* EOF *)
      | (exception
          Unix.(
            Unix_error
              ((ECONNRESET | ENOTCONN | EAGAIN | EWOULDBLOCK | EINTR), _, _))
          ) (* connection error or non-blocking socket *) ->
          consumed
      | n ->
          assert (n >= 0) ;
          assert (n <= len) ;
          loop (consumed + n)
  in
  loop 0

let repeat_write op fd buf off len =
  let rec loop written =
    let off = off + written and len = len - written in
    if len = 0 then
      written (* we've written the entire buffer *)
    else
      match op fd buf off len with
      | 0
        (* should never happen, but we cannot retry now or we'd enter an infinite loop *)
      | (exception
          Unix.(
            Unix_error
              ( ( ECONNRESET
                | EPIPE
                | EINTR
                | ENETDOWN
                | ENETUNREACH
                | EAGAIN
                | EWOULDBLOCK )
              , _
              , _
              ))
          ) (* connection error or nonblocking socket *) ->
          written
      | n ->
          assert (n >= 0) ;
          assert (n <= len) ;
          loop (written + n)
  in
  loop 0

module For_test = struct
  let unsafe_fd_exn t = Safefd.unsafe_to_file_descr_exn t.fd
end
