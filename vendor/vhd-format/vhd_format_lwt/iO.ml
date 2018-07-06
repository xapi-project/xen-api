(*
 * Copyright (C) 2011-2013 Citrix Inc
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

let debug_io = ref false

let complete name offset op fd buffer =
  let open Lwt in
  let ofs = buffer.Cstruct.off in
  let len = buffer.Cstruct.len in
  let buf = buffer.Cstruct.buffer in
  let rec loop acc fd buf ofs len =
    op fd buf ofs len >>= fun n ->
    let len' = len - n in
    let acc' = acc + n in
    if len' = 0 || n = 0
    then return acc'
    else loop acc' fd buf (ofs + n) len' in
  loop 0 fd buf ofs len >>= fun n ->
  if !debug_io
  then Printf.fprintf stderr "%s offset=%s buffer = [%s](%d)\n%!"
    name (match offset with Some x -> Int64.to_string x | None -> "None")
    (if Cstruct.len buffer > 16
     then (String.escaped (Cstruct.(to_string (sub buffer 0 13)))) ^ "..."
     else (String.escaped (Cstruct.to_string buffer)))
    (Cstruct.len buffer);
  if n = 0 && len <> 0
  then fail End_of_file
  else return ()

module Fd = struct
  open Lwt

  type fd = {
    fd: Lwt_unix.file_descr;
    filename: string;
    lock: Lwt_mutex.t;
  }

  let openfile filename rw =
    let unix_fd = File.openfile filename rw 0o644 in
    let fd = Lwt_unix.of_unix_file_descr unix_fd in
    let lock = Lwt_mutex.create () in
    return { fd; filename; lock }

  let fsync { fd = fd } =
    let fd' = Lwt_unix.unix_file_descr fd in
    File.fsync fd'

  let size_of_file t =
    Lwt_unix.LargeFile.fstat t.fd >>= fun s ->
    return s.Lwt_unix.LargeFile.st_size

  let create filename =
    (* First create the file as normal *)
    Lwt_unix.openfile filename [ Unix.O_RDWR; Unix.O_CREAT; Unix.O_TRUNC ] 0o644 >>=
    Lwt_unix.close >>= fun () ->
    (* Then re-open using our new API *)
    openfile filename true

  let close t = Lwt_unix.close t.fd

  exception Not_sector_aligned of int64

  let assert_sector_aligned n =
    if Int64.(mul(div n 512L) 512L) <> n then begin
      Printf.fprintf stderr "ERROR: %Ld not sector aligned\n%!" n;
      raise (Not_sector_aligned n)
    end

  let really_read { fd; filename; lock } offset (* in file *) buf =
    (* All reads and writes should be sector-aligned *)
    assert_sector_aligned offset;
    assert_sector_aligned (Int64.of_int buf.Cstruct.off);
    assert_sector_aligned (Int64.of_int (Cstruct.len buf));

    Lwt_mutex.with_lock lock
      (fun () ->
        Lwt.catch (fun () ->
          Lwt_unix.LargeFile.lseek fd offset Unix.SEEK_SET >>= fun _ ->
          complete "read" (Some offset) Lwt_bytes.read fd buf
        ) (function
        | Unix.Unix_error(Unix.EINVAL, "read", "") as e ->
          Printf.fprintf stderr "really_read offset = %Ld len = %d: EINVAL (alignment?)\n%!" offset (Cstruct.len buf);
          fail e
        | End_of_file as e ->
          Printf.fprintf stderr "really_read offset = %Ld len = %d: End_of_file\n%!" offset (Cstruct.len buf);
          fail e
        | e ->
          Printf.fprintf stderr "really_read offset = %Ld len = %d: %s\n%!"
            offset (Cstruct.len buf) (Printexc.to_string e);
          fail e
        )
      )

  let really_write { fd; filename; lock } offset (* in file *) buf =
    (* All reads and writes should be sector-aligned *)
    assert_sector_aligned offset;
    assert_sector_aligned (Int64.of_int buf.Cstruct.off);
    assert_sector_aligned (Int64.of_int (Cstruct.len buf));

    Lwt_mutex.with_lock lock
      (fun () ->
        Lwt.catch (fun () ->
          Lwt_unix.LargeFile.lseek fd offset Unix.SEEK_SET >>= fun _ ->
          complete "write" (Some offset) Lwt_bytes.write fd buf
        ) (function
        | Unix.Unix_error(Unix.EINVAL, "write", "") as e ->
          Printf.fprintf stderr "really_write offset = %Ld len = %d: EINVAL (alignment?)\n%!" offset (Cstruct.len buf);
          fail e
        | End_of_file as e ->
          Printf.fprintf stderr "really_write offset = %Ld len = %d: End_of_file\n%!" offset (Cstruct.len buf);
          fail e
        | e ->
          Printf.fprintf stderr "really_write offset = %Ld len = %d: %s\n%!"
            offset (Cstruct.len buf) (Printexc.to_string e);
          fail e
        )
      )

  let lseek { fd } ofs cmd = Lwt_unix.LargeFile.lseek fd ofs cmd
  let lseek_data { fd } ofs = Lwt_preemptive.detach (File.lseek_data (Lwt_unix.unix_file_descr fd)) ofs
  let lseek_hole { fd } ofs = Lwt_preemptive.detach (File.lseek_hole (Lwt_unix.unix_file_descr fd)) ofs
end

module IO = struct
  type 'a t = 'a Lwt.t

  let (>>=) = Lwt.(>>=)
  let return = Lwt.return
  let fail = Lwt.fail

  let exists path = return (try ignore(Unix.LargeFile.stat path); true with _ -> false)

  let y2k = 946684800.0 (* seconds from the unix epoch to the vhd epoch *)

  let get_vhd_time time =
    Int32.of_int (int_of_float (time -. y2k))

  let now () =
    let time = Unix.time() in
    get_vhd_time time

  let get_modification_time x =
    Lwt_unix.LargeFile.stat x >>= fun st ->
    return (get_vhd_time (st.Lwt_unix.LargeFile.st_mtime))

  let get_file_size x =
    try return (File.get_file_size x)
    with e -> fail e

  include Fd
end

include IO

let to_file_descr x = x.Fd.fd
