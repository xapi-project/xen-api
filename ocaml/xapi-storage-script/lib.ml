(* Copyright (C) Cloud Software Group Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License as published
   by the Free Software Foundation; version 2.1 only. with the special
   exception on linking described in file LICENSE.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Lesser General Public License for more details.
*)

module StringMap = Map.Make (String)

module Types = struct
  type backtrace = {
      error: string
    ; (* Python json.dumps and rpclib are not very friendly *)
      files: string list
    ; lines: int list
  }
  [@@deriving rpc]

  (* This matches xapi.py:exception *)
  type error = {code: string; params: string list; backtrace: backtrace}
  [@@deriving rpc]
end

let ( >>= ) = Lwt.bind

let return = Lwt_result.return

let fail = Lwt_result.fail

let ( // ) = Filename.concat

module Sys = struct
  type file = Regular | Directory | Other | Missing | Unknown

  let file_kind ~follow_symlinks path =
    Lwt.try_bind
      (fun () ->
        ( if follow_symlinks then
            Lwt_unix.LargeFile.stat
          else
            Lwt_unix.LargeFile.lstat
        )
          path
      )
      (function
        | s -> (
          match s.Unix.LargeFile.st_kind with
          | Unix.S_REG ->
              Lwt.return Regular
          | Unix.S_DIR ->
              Lwt.return Directory
          | _ ->
              Lwt.return Other
        )
        )
      (function
        | Unix.Unix_error (Unix.ENOENT, _, _) ->
            Lwt.return Missing
        | Unix.Unix_error ((Unix.EACCES | Unix.ELOOP), _, _) ->
            Lwt.return Unknown
        | e ->
            Lwt.fail e
        )

  let access path modes =
    Lwt.try_bind
      (fun () -> Lwt_unix.access path modes)
      Lwt_result.return
      (fun exn -> fail (`not_executable (path, exn)))

  let assert_is_executable path =
    file_kind ~follow_symlinks:true path >>= function
    | Directory | Other | Missing | Unknown ->
        fail (`missing path)
    | Regular -> (
        access path [Unix.X_OK] >>= function
        | Error exn ->
            fail exn
        | Ok () ->
            return ()
      )

  let read_file_contents path =
    Lwt_io.(with_file ~mode:input ~flags:[O_RDONLY] ~perm:0o000 path read)

  let save ~contents path =
    Lwt_io.(with_file ~mode:output path (Fun.flip write contents))

  let readdir path =
    path |> Lwt_unix.files_of_directory |> Lwt_stream.to_list >>= fun listing ->
    List.filter (function "." | ".." -> false | _ -> true) listing
    |> Lwt.return

  let mkdir_p ?(perm = 0o755) path =
    let rec loop acc path =
      let create_dir () = Lwt_unix.mkdir path perm in
      let create_subdirs () = Lwt_list.iter_s (fun f -> f ()) acc in
      Lwt.try_bind create_dir create_subdirs (function
        | Unix.(Unix_error (EEXIST, _, _)) ->
            (* create directories, parents first *)
            create_subdirs ()
        | Unix.(Unix_error (ENOENT, _, _)) ->
            let parent = Filename.dirname path in
            loop (create_dir :: acc) parent
        | exn ->
            let msg =
              Printf.sprintf {|Could not create directory "%s" because: %s|}
                path (Printexc.to_string exn)
            in
            Lwt.fail (Failure msg)
        )
    in
    loop [] path
end

module Signal = struct
  type t = int

  let to_string s = Fmt.(str "%a" Dump.signal s)
end

module Process = struct
  module Output = struct
    type exit_or_signal = Exit_non_zero of int | Signal of Signal.t

    type t = {
        exit_status: (unit, exit_or_signal) Result.t
      ; pid: int
      ; stdout: string
      ; stderr: string
    }

    let exit_or_signal_of_unix = function
      | Unix.WEXITED 0 ->
          Ok ()
      | WEXITED n ->
          Error (Exit_non_zero n)
      | WSIGNALED n ->
          Error (Signal n)
      | WSTOPPED n ->
          Error (Signal n)
  end

  let with_process ~env ~prog ~args f =
    let args = Array.of_list (prog :: args) in
    let cmd = (prog, args) in

    let env =
      Unix.environment ()
      |> Array.to_seq
      |> Seq.map (fun kv ->
             let k, v = Scanf.sscanf kv "%s@=%s" (fun k v -> (k, v)) in
             (k, v)
         )
      |> StringMap.of_seq
      |> StringMap.add_seq (List.to_seq env)
      |> StringMap.to_seq
      |> Seq.map (fun (k, v) -> Printf.sprintf "%s=%s" k v)
      |> Array.of_seq
    in

    Lwt_process.with_process_full ~env cmd f

  let close chan () = Lwt_io.close chan

  let send chan data =
    Lwt.finalize (fun () -> Lwt_io.write chan data) (close chan)

  let receive chan = Lwt.finalize (fun () -> Lwt_io.read chan) (close chan)

  let run ~env ~prog ~args ~input =
    let ( let@ ) f x = f x in
    let@ p = with_process ~env ~prog ~args in
    let pid = p#pid in
    let sender = send p#stdin input in
    let receiver_out = receive p#stdout in
    let receiver_err = receive p#stderr in
    Lwt.catch
      (fun () ->
        let receiver = Lwt.both receiver_out receiver_err in
        Lwt.both sender receiver >>= fun ((), (stdout, stderr)) ->
        p#status >>= fun status ->
        let exit_status = Output.exit_or_signal_of_unix status in
        Lwt.return {Output.exit_status; pid; stdout; stderr}
      )
      (function
        | Lwt.Canceled as exn ->
            Lwt.cancel receiver_out ; Lwt.cancel receiver_err ; Lwt.fail exn
        | exn ->
            Lwt.fail exn
        )
end

module DirWatcher = struct
  type event = Modified of string | Changed

  let create path =
    Lwt_inotify.create () >>= fun desc ->
    let watches = Hashtbl.create 32 in
    let selectors =
      Inotify.[S_Close; S_Create; S_Delete; S_Delete_self; S_Modify; S_Move]
    in
    Lwt_inotify.add_watch desc path selectors >>= fun watch ->
    (* Deduplicate the watches by removing the previous one from inotify and
       replacing it in the table *)
    let maybe_remove =
      if Hashtbl.mem watches watch then
        Lwt_inotify.rm_watch desc watch
      else
        Lwt.return_unit
    in
    maybe_remove >>= fun () ->
    Hashtbl.replace watches watch path ;
    Lwt.return (watches, desc)

  let read (watches, desc) =
    Lwt_inotify.read desc >>= fun (wd, mask, _cookie, filename) ->
    let overflowed =
      Inotify.int_of_watch wd = -1 && mask = [Inotify.Q_overflow]
    in
    let watch_path = Hashtbl.find_opt watches wd in
    match (overflowed, watch_path) with
    | true, _ ->
        Lwt.return [Changed]
    | _, None ->
        Lwt.return []
    | _, Some base_path ->
        let path =
          match filename with
          | None ->
              base_path
          | Some name ->
              base_path // name
        in

        List.filter_map
          (function
            | Inotify.Access
            | Attrib
            | Isdir
            | Open
            | Close_nowrite
            | Ignored
            | Unmount ->
                None
            | Close_write | Modify | Move_self ->
                Some (Modified path)
            | Create | Delete | Delete_self | Moved_from | Moved_to | Q_overflow
              ->
                Some Changed
            )
          mask
        |> Lwt.return
end

module Clock = struct let after ~seconds = Lwt_unix.sleep seconds end
