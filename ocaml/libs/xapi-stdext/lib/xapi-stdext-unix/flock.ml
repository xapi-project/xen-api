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

let prepare t =
  try
    Unixext.mkdir_safe t.dir 0o700 ;
    Result.map_error
      (function `Failed why -> why | `Absent -> t.dir ^ ": missing")
      (check_dir t)
  with Unix.Unix_error (e, _, _) -> Error (t.dir ^ ": " ^ Unix.error_message e)

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

let device_text st_dev =
  let d = Unixext.Stat.decode_st_dev st_dev in
  Printf.sprintf "%02x:%02x" d.major d.minor

let fdinfo fd =
  Filename.(concat (concat (dirname (dirname fd)) "fdinfo") (basename fd))

module Granted = struct
  type t = {pid: int; hold: hold; dev: string; ino: int}
end

(* From the kernel's line "1: FLOCK ADVISORY WRITE 1234 00:2a:5678 0 EOF" in
   /proc/locks, or "lock: 1: FLOCK ..." in an fdinfo; none for a waiter's
   line ("1: -> FLOCK"), nor for an inode too large for an int, as some file
   systems give *)
let flock_of_line line =
  let ( let* ) = Option.bind in
  match Astring.String.fields ~empty:false line with
  | "lock:" :: _ :: "FLOCK" :: _ :: rw :: pid :: file :: _
  | _ :: "FLOCK" :: _ :: rw :: pid :: file :: _ ->
      let* pid = int_of_string_opt pid in
      let* hold = List.assoc_opt rw [("WRITE", Write); ("READ", Read)] in
      let* dev, ino = Astring.String.cut ~rev:true ~sep:":" file in
      let* ino = int_of_string_opt ino in
      Some {Granted.pid; hold; dev; ino}
  | _ ->
      None

(* The flock of the open file of [fd], a link "/proc/<pid>/fd/<n>": its
   fdinfo shows it also for a child that kept the descriptor, under its
   taker's pid, where /proc/locks shows only the taker *)
let flock_of fd =
  let first found line =
    match found with None -> flock_of_line line | Some _ -> found
  in
  try Unixext.file_lines_fold first None (fdinfo fd) with Sys_error _ -> None

(* streamed, so that [stop] bounds a process with many descriptors *)
let fold_fds ~stop pid f init =
  let fds = Printf.sprintf "/proc/%d/fd/" pid in
  Unixext.with_directory fds @@ fun d ->
  let rec walk acc =
    match Unix.readdir d with
    | name ->
        if stop () then raise Exit ;
        let fd = fds ^ name in
        let target = try Unix.readlink fd with Unix.Unix_error _ -> "" in
        (walk [@tailcall]) (f acc fd target)
    | exception End_of_file ->
        acc
  in
  walk init

let ids dir = List.filter_map int_of_string_opt (Array.to_list (Sys.readdir dir))

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

let checked h ~links =
  [
    ("id", h.key)
  ; ("boot", h.lock.boot_id ())
  ; ("ino", string_of_int h.ino)
  ; ("links", string_of_int links)
  ]

let own_mark h = read_mark h.lock (self_fd_path h.fd)

let attested h =
  let links = (Unix.fstat h.fd).st_nlink in
  match own_mark h with
  | Ok kv when List.for_all (fun f -> List.mem f kv) (checked h ~links) ->
      Some kv
  | _ ->
      None

let write_mark h kv =
  let line =
    String.concat " " (h.lock.magic :: List.map (fun (k, v) -> k ^ "=" ^ v) kv)
    ^ "\n"
  in
  try
    Unix.ftruncate h.fd 0 ;
    ignore (Unix.lseek h.fd 0 SEEK_SET : int) ;
    Unixext.really_write_string h.fd line ;
    Ok (Unixext.fsync h.fd)
  with Unix.Unix_error (e, _, _) -> Error (Unix.error_message e)

let proc_number name file =
  let number found line =
    match Astring.String.cut ~sep:":" line with
    | Some (n, value) when n = name ->
        int_of_string_opt (String.trim value)
    | _ ->
        found
  in
  try Unixext.file_lines_fold number None file with Sys_error _ -> None

(* The mount of the file that link [fd] opens: no call on its file system *)
let mount_id fd = proc_number "mnt_id" (fdinfo fd)

let dir_mount t =
  Option.bind (Result.to_option (check_dir t)) @@ fun () ->
  Unixext.with_file t.dir [O_RDONLY; O_CLOEXEC; O_NONBLOCK; O_NOCTTY] 0
  @@ fun fd -> mount_id (self_fd_path fd)

(* [fd]'s inode, if [inode] accepts it and a taker would take it for [key]'s:
   the key's old name, a mark naming the key, or unnamed and marked with no
   key. Only the mount [mnt] is read: a file opened in another mount
   namespace, or through a bind mount, is missed *)
let key_inode t key (dir : Unix.stats) ~mnt ~inode fd target =
  let of_key () =
    match read_mark t fd with
    | Ok kv when List.mem_assoc "id" kv ->
        List.assoc_opt "id" kv = Some key
    | Ok _ | Error `Other_version ->
        String.starts_with ~prefix:(t.dir ^ "/#") target
  in
  if mount_id fd <> Some mnt then
    None
  else
    match Unix.stat fd with
    | st
      when (st.st_dev, st.st_uid) = (dir.st_dev, dir.st_uid)
           && st.st_perm land 0o022 = 0
           && inode st.st_ino
           && (target = path t key ^ " (deleted)" || of_key ()) ->
        Some st.st_ino
    | _ | (exception Unix.Unix_error _) ->
        None

(* Waiters are not in /proc/locks, as a taker re-checks its file once it
   holds it. Reading the file holds back the host's lock operations, so
   [stop] ends a long read *)
let flock_holds ~stop =
  let parse holds line =
    if stop () then raise Exit ;
    Option.fold ~none:holds ~some:(fun h -> h :: holds) (flock_of_line line)
  in
  Unixext.file_lines_fold parse [] "/proc/locks"

(* Holds seen in fdinfo, so also a child's kept descriptor, and older objects
   open (Open), which may be locked later; then each flock of /proc/locks that
   no descriptor showed, if its process has the file open as an object of
   [key], or cannot be read *)
let holds_of_key t key ~stop =
  match (Unix.stat t.dir, dir_mount t) with
  | exception Unix.Unix_error (e, _, _) ->
      Error (t.dir ^ ": " ^ Unix.error_message e)
  | _, None ->
      Error (t.dir ^ ": its mount is unknown")
  | dir, Some mnt -> (
      let current =
        try Some (Unix.lstat (path t key)).st_ino
        with Unix.Unix_error _ -> None
      in
      let device = device_text dir.st_dev in
      let prefix = t.dir ^ "/" in
      let opened pid =
        let inode ino = current <> Some ino in
        let add found fd target =
          if not (String.starts_with ~prefix target) then
            found
          else
            let lock = flock_of fd in
            match (current, lock) with
            | Some ino, Some g when (g.dev, g.ino) = (device, ino) ->
                (* under its name, or unnamed by its creator *)
                {g with pid} :: found
            | _ when target = path t key ->
                found
            | _ ->
                let hold =
                  Option.fold ~none:Open ~some:(fun g -> g.Granted.hold) lock
                in
                key_inode t key dir ~mnt ~inode fd target
                |> Option.fold ~none:found ~some:(fun ino ->
                    {Granted.pid; hold; dev= device; ino} :: found
                )
        in
        try fold_fds ~stop pid add [] with Unix.Unix_error _ -> []
      in
      try
        let seen = List.concat_map opened (ids "/proc") in
        let known = List.map (fun s -> (s.Granted.pid, s.ino)) seen in
        let held {Granted.pid; dev; ino; _} =
          let is_it found fd target =
            found
            || key_inode t key dir ~mnt ~inode:(( = ) ino) fd target <> None
          in
          dev = device
          && current <> Some ino
          && (not (List.mem (pid, ino) known))
          &&
            try fold_fds ~stop pid is_it false
            with Unix.Unix_error _ ->
              Sys.file_exists (Printf.sprintf "/proc/%d" pid)
        in
        Ok (List.sort_uniq compare (List.filter held (flock_holds ~stop) @ seen))
      with Exit -> Error "listing the holders ran out of time"
    )

(* Field [n] of the stat file in the /proc directory [d] of a process or
   thread, after the command name: 0 is the state, 19 the start time *)
let stat_field n d =
  match
    Astring.String.cut ~rev:true ~sep:") "
      (Unixext.string_of_file (Filename.concat d "stat"))
  with
  | Some (_, fields) ->
      List.nth_opt (String.split_on_char ' ' fields) n
  | None | (exception Unix.Unix_error _) ->
      None

let d_threads pid =
  let tasks = Printf.sprintf "/proc/%d/task/" pid in
  let in_d tid = stat_field 0 (tasks ^ string_of_int tid ^ "/") = Some "D" in
  try List.filter in_d (ids tasks) with Sys_error _ -> []

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

let incarnation h = Printf.sprintf "inode %d" h.ino

let release h = Unix.close h.fd

let state h =
  match Unix.lstat (path h.lock h.key) with
  | st when st.st_kind = S_REG && (st.st_dev, st.st_ino) = (h.dev, h.ino) ->
      Option.fold ~none:Unattested ~some:(fun _ -> Attested) (attested h)
  | _ | (exception Unix.Unix_error _) ->
      Replaced

(* The key's name in a new file before it has one: a census of another key
   then does not count its creator *)
let fresh t key =
  match Result.map (fun () -> Unixext.open_tmpfile t.dir) (check_dir t) with
  | Error _ ->
      Error (`Failed (t.dir ^ ": cannot hold a new lock file"))
  | exception Unix.Unix_error (e, _, _) ->
      Error (`Failed (t.dir ^ ": " ^ Unix.error_message e))
  | Ok fd -> (
      let st = Unix.fstat fd in
      let h = {lock= t; key; fd; dev= st.st_dev; ino= st.st_ino} in
      let publish () = Unix.link ~follow:true (self_fd_path fd) (path t key) in
      match
        if Unixext.flock_try fd ~exclusive:true then
          Result.map publish (write_mark h [("v", "1.0"); ("id", key)])
        else
          Error "a new unnamed file was locked by another process"
      with
      | Ok () ->
          Ok h
      | Error why ->
          Unix.close fd ;
          Error (`Failed why)
      | exception Unix.Unix_error (EEXIST, _, _) ->
          Unix.close fd ; Error `Exists
      | exception Unix.Unix_error (e, _, _) ->
          Unix.close fd ;
          Error (`Failed ("linkat: " ^ Unix.error_message e))
    )

(* The mark's fields of this module; the others of an old mark are kept *)
let own = ["v"; "id"; "boot"; "ino"; "links"; "nonce"; "label"]

let attest_if_alone t h waiter =
  let older g = g.Granted.ino <> h.ino in
  let kept kv = List.filter (fun (k, _) -> not (List.mem k own)) kv in
  Result.bind (holds_of_key t h.key ~stop:waiter.stop) @@ fun holds ->
  match List.filter older holds with
  | [] -> (
    match own_mark h with
    | Error `Other_version ->
        Error "another version's mark is never overwritten"
    | Ok kv ->
        let rand = Random.State.bits64 (Random.State.make_self_init ()) in
        write_mark h
          ((("v", "1.0") :: checked h ~links:1)
          @ (("nonce", Printf.sprintf "%016Lx" rand) :: kept kv)
          @ [("label", "0")]
          )
  )
  | others ->
      Error
        (Printf.sprintf "%d hold(s) on an older lock file" (List.length others))

let holders t key waiter =
  let holder {Granted.pid; hold; dev; ino} =
    let proc = Printf.sprintf "/proc/%d/" pid in
    let command =
      try String.trim (Unixext.string_of_file (proc ^ "comm"))
      with Unix.Unix_error _ -> "?"
    in
    let run_state = Option.value ~default:"?" (stat_field 0 proc) in
    let file = Printf.sprintf "%s:%d" dev ino in
    {pid; command; run_state; hold; file; d_threads= d_threads pid}
  in
  Result.map (List.map holder) (holds_of_key t key ~stop:waiter.stop)

let fds_for_child h = [h.fd]
