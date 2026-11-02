(*
 * Copyright (C) 2026 Cloud Software Group
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

open Lock_rw

type t = {dir: string; magic: string; boot_id: unit -> string}

type 'mode held = {
    lock: t
  ; key: string
  ; fd: Unix.file_descr
  ; dev: int
  ; ino: int
}

let create ~boot_id ~magic dir = {dir; magic; boot_id}

let path t key = Filename.concat t.dir (key ^ ".lock")

let self_fd_path fd =
  Printf.sprintf "/proc/self/fd/%d" (Unixext.int_of_file_descr fd)

(* the kernel names files by their canonical path, which the census compares *)
let check_dir t =
  match Unix.lstat t.dir with
  | {Unix.st_kind= S_DIR; st_uid; st_perm= 0o700; _}
    when st_uid = Unix.geteuid () && Unix.realpath t.dir = t.dir ->
      Ok ()
  | _ ->
      Error (`Failed (t.dir ^ ": not a directory of mode 0700 of this user"))
  | exception Unix.Unix_error (ENOENT, _, _) ->
      Error `Absent
  | exception Unix.Unix_error (e, _, _) ->
      Error (`Failed (t.dir ^ ": " ^ Unix.error_message e))

(* Checked before opening, so a link, a FIFO or a device is never opened; the
   inode comparison closes the race *)
let open_checked (type m) t key (mode : m mode) =
  let path = path t key in
  match Unix.lstat path with
  | {st_kind= S_REG; st_uid; st_perm; st_dev= dev; st_ino= ino; _}
    when st_uid = Unix.geteuid () && st_perm land 0o022 = 0 -> (
      let access =
        match mode with Shared -> Unix.O_RDONLY | Exclusive -> O_RDWR
      in
      let fd = Unix.openfile path [access; O_CLOEXEC; O_NONBLOCK; O_NOCTTY] 0 in
      match Unix.fstat fd with
      | opened when (opened.st_dev, opened.st_ino) = (dev, ino) ->
          Ok {lock= t; key; fd; dev; ino}
      | _ ->
          Unix.close fd ;
          Error (`Failed (path ^ ": replaced while opening"))
      | exception e ->
          Unixext.raise_with_preserved_backtrace e (fun () -> Unix.close fd)
    )
  | _ ->
      Error (`Failed (path ^ ": not a regular file writable only by this user"))

let distinct names =
  List.sort_uniq String.compare names = List.sort String.compare names

(* The mark: one line "<magic> v=1.0 name=value ...", read as its fields. A
   name written twice makes it no mark: no fields; another major version is not
   ours and is never rewritten. *)
let read_mark t path =
  let text = Unixext.read_small_regular_file ~max_bytes:4096 path in
  let line = Option.map String.trim (Result.value ~default:None text) in
  match Option.map (String.split_on_char ' ') line with
  | Some (first :: fields) when first = t.magic -> (
      let kv = List.filter_map (Astring.String.cut ~sep:"=") fields in
      match
        Option.bind (List.assoc_opt "v" kv) (Astring.String.cut ~sep:".")
      with
      | Some ("1", _) when distinct (List.map fst kv) ->
          Ok kv
      | Some ("1", _) ->
          Ok []
      | _ ->
          Error `Other_version
    )
  | _ ->
      Ok []

(* The fields that tie a mark to its file: the key, this boot, the inode and
   the link count *)
let identity_fields h ~links =
  [
    ("id", h.key)
  ; ("boot", h.lock.boot_id ())
  ; ("ino", string_of_int h.ino)
  ; ("links", string_of_int links)
  ]

let own_mark h = read_mark h.lock (self_fd_path h.fd)

let attested h =
  let identity = identity_fields h ~links:(Unix.fstat h.fd).st_nlink in
  match own_mark h with
  | Ok kv when List.for_all (fun f -> List.mem f kv) identity ->
      Some kv
  | _ ->
      None

let acquire (type m) t key (mode : m mode) waiter :
    (m held, acquire_error) result =
  let exclusive = match mode with Exclusive -> true | Shared -> false in
  let lock h =
    let retry () = Unixext.flock_try h.fd ~exclusive in
    match
      if retry () then
        Ok ()
      else
        waiter.wait ~retry ~blockers:(fun () -> [])
    with
    | Ok () ->
        Ok h
    | Error e ->
        Unix.close h.fd ;
        Error (e :> acquire_error)
    | exception e ->
        Unixext.raise_with_preserved_backtrace e (fun () -> Unix.close h.fd)
  in
  match Result.bind (check_dir t) (fun () -> open_checked t key mode) with
  | opened ->
      Result.bind opened lock
  | exception Unix.Unix_error (ENOENT, _, _) ->
      Error `Absent
  | exception Unix.Unix_error (e, _, _) ->
      Error (`Failed (path t key ^ ": " ^ Unix.error_message e))

let release h = Unix.close h.fd

(* Whether the key's name still leads to the held file *)
let is_current h =
  match Unix.lstat (path h.lock h.key) with
  | st ->
      st.st_kind = S_REG && (st.st_dev, st.st_ino) = (h.dev, h.ino)
  | exception Unix.Unix_error _ ->
      false

let state h =
  if not (is_current h) then
    Replaced
  else
    Option.fold ~none:Unattested ~some:(fun _ -> Attested) (attested h)

let fds_for_child h = [h.fd]
