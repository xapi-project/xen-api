(* Copyright (C) Cloud Software Group, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License as published
   by the Free Software Foundation; version 2.1 only. with the special
   exception on linking described in file LICENSE.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Lesser General Public License for more details.
*)

module D = Debug.Make (struct let name = __MODULE__ end)

let ( // ) = Filename.concat

let runtime_data = "/var/lib" // "xapi-guard"

let ( let* ) = Lwt.bind

let ( let@ ) f x = f x

let with_lock = Lwt_mutex.with_lock

type t = Uuidm.t * Mtime.t * Types.Tpm.key

let cache_of service = runtime_data // Types.Service.to_string service

let fistpoint () =
  let name = "/tmp/fist_disable_xapi_guard_cache" in
  Lwt.catch
    (fun () ->
      let* () = Lwt_unix.access name [Unix.F_OK] in
      Lwt.return true
    )
    (fun _ -> Lwt.return false)

let files_in dir ~otherwise =
  Lwt.catch
    (fun () ->
      let* listing = Lwt_unix.files_of_directory dir |> Lwt_stream.to_list in
      List.filter_map
        (function "." | ".." -> None | name -> Some (dir // name))
        listing
      |> Lwt.return
    )
    otherwise

let unlink_safe file =
  let __FUN = __FUNCTION__ in
  Lwt.catch
    (fun () -> Lwt_unix.unlink file)
    (function
      | Unix.(Unix_error (ENOENT, _, _)) ->
          Lwt.pause ()
      | e ->
          D.info "%s: error %s when deleting %s, ignoring" __FUN
            (Printexc.to_string e) file ;
          Lwt.pause ()
      )

type valid_file = t * string

type file = Latest of valid_file | Outdated of valid_file | Invalid of string

let print_key (uuid, timestamp, key) =
  Uuidm.to_string uuid
  // Types.Tpm.(serialize_key key |> string_of_int)
  // Mtime.(to_uint64_ns timestamp |> Int64.to_string)

let path_of_key root key = root // print_key key

let key_of_path path =
  let ( let* ) = Option.bind in
  let key_dir = Filename.(dirname path) in
  let* uuid = Filename.(basename (dirname key_dir)) |> Uuidm.of_string in
  let* key =
    Filename.basename key_dir
    |> int_of_string_opt
    |> Option.map (fun e ->
           Types.Tpm.deserialize_key e
           |> Result.map_error (fun msg -> D.info "Invalid key found: %s" msg)
           |> Result.to_option
       )
    |> Option.join
  in
  let* timestamp =
    Filename.basename path
    |> Int64.of_string_opt
    |> Option.map Mtime.of_uint64_ns
  in
  Some ((uuid, timestamp, key), path)

let only_latest = function
  | Latest f ->
      Either.Left f
  | Outdated (_, p) | Invalid p ->
      Right p

let sort_updates contents =
  let classify elem =
    match key_of_path elem with
    | None ->
        Either.Right (Invalid elem)
    | Some valid_file ->
        Either.Left valid_file
  in
  let valid_files, invalid = List.partition_map classify contents in

  let valid =
    let ordered =
      List.fast_sort
        (fun ((_, x, _), _) ((_, y, _), _) -> Mtime.compare y x)
        valid_files
    in
    match ordered with
    | [] ->
        []
    | latest :: outdated ->
        Latest latest :: List.map (fun outdated -> Outdated outdated) outdated
  in
  List.concat [valid; invalid]

let get_all_contents root =
  let empty = Fun.const (Lwt.return []) in
  let contents_of_key key =
    let* contents = files_in key ~otherwise:empty in
    Lwt.return (sort_updates contents)
  in
  let* tpms = files_in root ~otherwise:empty in
  let* files =
    Lwt_list.map_p
      (fun tpm ->
        let* keys = files_in tpm ~otherwise:empty in
        Lwt_list.map_p contents_of_key keys
      )
      tpms
  in
  Lwt.return List.(concat (concat files))

(** Warning, may raise Unix.Unix_error *)
let read_from ~filename =
  let flags = Unix.[O_RDONLY] in
  let perm = 0o000 in
  Lwt_io.with_file ~flags ~perm ~mode:Input filename Lwt_io.read

let persist_to ~filename:f_path ~contents =
  let atomic_write_to_file ~perm f =
    let tmp_path = f_path ^ ".pre" in
    let dirname = Filename.dirname f_path in
    let flags = Unix.[O_WRONLY; O_CREAT; O_SYNC] in
    let* fd_tmp = Lwt_unix.openfile tmp_path flags perm in
    let* () =
      Lwt.finalize
        (fun () ->
          (* do not close fd when closing the channel, avoids double-closing the fd *)
          let close () = Lwt.return_unit in
          let chan = Lwt_io.of_fd ~mode:Output ~close fd_tmp in
          let* () =
            Lwt.finalize (fun () -> f chan) (fun () -> Lwt_io.close chan)
          in
          Lwt_unix.fsync fd_tmp
        )
        (fun () -> Lwt_unix.close fd_tmp)
    in
    let* () = Lwt_unix.rename tmp_path f_path in
    let* fd_dir = Lwt_unix.openfile dirname [O_RDONLY] 0 in
    Lwt.finalize
      (fun () -> Lwt_unix.fsync fd_dir)
      (fun () -> Lwt_unix.close fd_dir)
  in
  let write out_chan = Lwt_io.write out_chan contents in
  atomic_write_to_file ~perm:0o600 write

(** - Direct: request doesn't pass through the cache
    - Engaged: both side coordinate through the queue, writer ends the mode
      when the queue has been filled.
    - Disengaged: writer ignores the queue, reader empties it and the cache;
      then it changes the mode to engaged.
*)
type state = Direct | Engaged | Disengaged

type channel = {
    queue: t Lwt_bounded_stream.t
  ; push: t option -> unit option
  ; lock: Lwt_mutex.t (* lock for the states *)
  ; mutable state: state
}

(*
   Notes:
   - uses Mtime.t to force usage of monotonic time
   - This means that between runs (and reboots) cached stated is lost if not
     persisted first.
   IDEA: carryover: read contents of cache and "convert it" to the current run

   TODO:
     - Exponential backoff on xapi push error
     - Limit error logging on xapi push error: once per downtime is enough
 *)

module Writer : sig
  val with_cache :
       direct:
         (t -> (string, exn) Lwt_result.t)
         * (t -> string -> (unit, exn) Lwt_result.t)
    -> Types.Service.t
    -> channel
    -> ((t -> string Lwt.t) * (t -> string -> unit Lwt.t) -> 'a Lwt.t)
    -> 'a Lwt.t
  (** [with_cache ~direct typ queue context] creates a cache for content of
      type [typ]. The cache is readable and writable through the function
      [context], which is provided a reading and writing functions [direct].
      It uses [channel] to push events to

      Example:
        Xapi_guard.Disk_cache.(Writer.with_cache ~direct:(read, upload) Tpm channel)
        @@ fun read_tpm, write_tpm -> write_tpm (uuid, time, key) contents
    *)
end = struct
  let mkdir_p ?(perm = 0o755) path =
    let rec loop acc path =
      let create_dir () = Lwt_unix.mkdir path perm in
      let create_subdirs () = Lwt_list.iter_s (fun (_, f) -> f ()) acc in
      Lwt.try_bind create_dir create_subdirs (function
        | Unix.(Unix_error (EEXIST, _, _)) ->
            (* create directories, parents first *)
            create_subdirs ()
        | Unix.(Unix_error (ENOENT, _, _)) ->
            let parent = Filename.dirname path in
            loop ((path, create_dir) :: acc) parent
        | exn ->
            let msg =
              Printf.sprintf {|Could not create directory "%s" because: %s|}
                path (Printexc.to_string exn)
            in
            Lwt.fail (Failure msg)
        )
    in
    loop [] path

  let files_in_existing dir =
    let create_dir = function
      | Unix.(Unix_error (ENOENT, _, _)) ->
          let* () = mkdir_p dir ~perm:0o700 in
          Lwt.return []
      | e ->
          raise e
    in
    files_in dir ~otherwise:create_dir

  let fail exn =
    Debug.log_backtrace exn (Backtrace.get exn) ;
    Lwt_result.fail exn

  let read_contents ~direct root (uuid, now, key) =
    let read_remote () =
      let read, _ = direct in
      let* result =
        Lwt.try_bind
          (fun () -> read (uuid, now, key))
          (function
            | Ok contents -> Lwt_result.return contents | Error exn -> fail exn
            )
          fail
      in
      match result with
      | Ok contents ->
          Lwt.return contents
      | Error exn ->
          raise exn
    in

    let key_str = Types.Tpm.(serialize_key key |> string_of_int) in
    let key_dir = root // Uuidm.(to_string uuid) // key_str in

    (* 1. Get updates *)
    let* contents = files_in key_dir ~otherwise:(fun _ -> Lwt.return []) in
    let updates = sort_updates contents in

    (* 2. Pick latest *)
    let latest, _ = List.partition_map only_latest updates in

    (* 3. fall back to remote read if needed *)
    let get_contents (_, path) =
      Lwt.catch (fun () -> read_from ~filename:path) (fun _ -> read_remote ())
    in

    match latest with path :: _ -> get_contents path | [] -> read_remote ()

  let write_contents ~direct root queue (uuid, now, key) contents =
    let __FUN = __FUNCTION__ in

    let _, direct = direct in
    let key_str = Types.Tpm.(serialize_key key |> string_of_int) in
    let key_dir = root // Uuidm.(to_string uuid) // key_str in
    (* 1. Record existing requests in cache *)
    let* outdated_contents = files_in_existing key_dir in

    let filename = key_dir // (Mtime.to_uint64_ns now |> Int64.to_string) in

    (* 2. Try to push the changes, if possible. If it's not possible because of
       the mode or a failure, write new timestamped content to cache,
       atomically; and finally notify the other side if needed *)
    (* Note that all queue operations must use while holding its mutex *)
    let persist () = persist_to ~filename ~contents in
    let persist_and_push () =
      let push () =
        match queue.push (Some (uuid, now, key)) with
        | Some () ->
            Lwt.return_unit
        | None ->
            (* Queue is full, change mode to ignore queue *)
            queue.state <- Disengaged ;
            Lwt.return_unit
      in
      let* () = persist () in
      push ()
    in
    let engage_and_persist exn =
      queue.state <- Engaged ;
      D.info "%s: Error on push. Reason: %s" __FUN (Printexc.to_string exn) ;
      let* () = persist_and_push () in
      Lwt_result.return ()
    in
    let read_state_and_push on_exception () =
      match queue.state with
      | Direct ->
          let* result =
            Lwt.try_bind
              (fun () -> direct (uuid, now, key) contents)
              (function
                | Ok () -> Lwt_result.return () | Error exn -> on_exception exn
                )
              on_exception
          in
          Lwt.return result
      | Engaged ->
          let* () = persist_and_push () in
          Lwt_result.return ()
      | Disengaged ->
          let* () = persist () in
          Lwt_result.return ()
    in
    let* cache_disabled = fistpoint () in
    let on_exception = if cache_disabled then fail else engage_and_persist in

    let* result = with_lock queue.lock (read_state_and_push on_exception) in
    let* () =
      match result with Ok () -> Lwt.return_unit | Error exn -> raise exn
    in

    (* 4. Delete previous requests from filesystem *)
    let* _ = Lwt_list.map_p unlink_safe outdated_contents in
    Lwt.return_unit

  let with_cache ~direct typ queue f =
    let root = cache_of typ in
    let* () = mkdir_p root ~perm:0o700 in
    f (read_contents ~direct root, write_contents ~direct root queue)
end

module Watcher : sig
  val watch :
       direct:(t -> string -> (unit, exn) Lwt_result.t)
    -> Types.Service.t
    -> channel
    -> unit
    -> unit Lwt.t
end = struct
  type push_cache = File of valid_file | Update_all | Wait

  let get_latest_and_delete_rest root =
    let* files = get_all_contents root in
    let latest, to_delete = List.partition_map only_latest files in
    let* () = Lwt_list.iter_p unlink_safe to_delete in
    Lwt.return latest

  let retry_push push (uuid, timestamp, key) contents =
    let __FUN = __FUNCTION__ in
    let push' () = push (uuid, timestamp, key) contents in
    let counter = Mtime_clock.counter () in
    let rec retry is_first_try =
      let on_error e =
        if is_first_try then
          D.debug "%s: Error on push, retrying. Reason: %s" __FUN
            (Printexc.to_string e) ;
        let* () = Lwt_unix.sleep 0.1 in
        retry false
      in
      Lwt.try_bind push'
        (function
          | Ok () -> Lwt.return (not is_first_try) | Error e -> on_error e
          )
        on_error
    in
    let* failed = retry true in
    ( if failed then
        let elapsed = Mtime_clock.count counter in
        D.debug "%s: Pushed %s after trying for %s" __FUN
          (print_key (uuid, timestamp, key))
          (Fmt.to_to_string Mtime.Span.pp elapsed)
    ) ;
    Lwt.return_unit

  let push_file push (key, path) =
    let __FUN = __FUNCTION__ in
    let on_error = function
      | Unix.(Unix_error (ENOENT, _, _)) ->
          Lwt.return_unit
      | exn ->
          D.info "%s: error when reading '%s': %s" __FUN path
            Printexc.(to_string exn) ;
          Lwt.return_unit
    in

    Lwt.try_bind
      (fun () -> read_from ~filename:path)
      (fun contents ->
        let* () = retry_push push key contents in
        unlink_safe path
      )
      on_error

  let push_files push files = Lwt_list.iter_s (push_file push) (List.rev files)

  let update_all queue push root =
    let __FUN = __FUNCTION__ in
    let* contents = get_latest_and_delete_rest root in
    let* () = push_files push contents in
    let@ () = with_lock queue.lock in
    let* contents = get_latest_and_delete_rest root in
    let* () =
      match contents with
      | [] ->
          queue.state <- Direct ;
          D.debug "%s: Cache clean; Going direct" __FUN ;
          Lwt.return_unit
      | _ ->
          Lwt.return_unit
    in
    Lwt.return_unit

  let resolve queue push root = function
    | File file -> (
        let* () = push_file push file in
        let@ () = with_lock queue.lock in
        match queue.state with
        | Direct | Disengaged ->
            Lwt.return_unit
        | Engaged ->
            let () =
              if Lwt_bounded_stream.size queue.queue = 0 then
                queue.state <- Direct
            in
            Lwt.return_unit
      )
    | Update_all ->
        update_all queue push root
    | Wait ->
        (* Do not busy loop when the system can cope with the requests *)
        Lwt_unix.sleep 0.2

  let watch ~direct typ queue =
    let root = cache_of typ in
    let __FUN = __FUNCTION__ in
    let rec loop () =
      (* When the pushing side is disengaged it doesn't push events to the
         queue, this means that trying to drain it completely would leave the
         pulling side locked waiting when the queue is empty.
           - Read the number of elements in the queue while draining it and
             then switch to read the contents from the cache; or
           - Switch immediately to reading the contents from cache and ignore
             the contents of the queue by calling an specialized method in the
             queue module to drain it.
      *)
      let get_action () =
        let@ () = with_lock queue.lock in
        match queue.state with
        | Disengaged when Lwt_bounded_stream.size queue.queue < 1 ->
            let* () = Lwt.pause () in
            Lwt.return Update_all
        | Direct ->
            let* () = Lwt.pause () in
            Lwt.return Wait
        | _ -> (
            let* elem = Lwt_bounded_stream.get queue.queue in
            match elem with
            | None ->
                raise (Failure "Other side closed channel, cannot continue")
            | Some elem ->
                Lwt.return (File (elem, path_of_key root elem))
          )
      in
      let* action = get_action () in
      let* () = resolve queue direct root action in
      loop ()
    in
    loop
end

(** Module use to change the cache contents before the reader and writer start
    running *)
module Setup : sig
  val retime_cache_contents : Types.Service.t -> t List.t Lwt.t
  (** [retime_cache_contents typ] retimes the current cache contents so they
      are time congruently with the current execution and returns the keys of
      valid files that are yet to be pushed *)
end = struct
  type file_action =
    | Keep of file
    | Delete of string
    | Move of {from: string; into: string}

  let get_fs_action root now acc = function
    | Latest ((uuid, timestamp, key), from) as latest ->
        if Mtime.is_later ~than:now timestamp then
          let timestamp = now in
          let into = path_of_key root (uuid, timestamp, key) in
          ((uuid, timestamp, key) :: acc, Move {from; into})
        else
          ((uuid, timestamp, key) :: acc, Keep latest)
    | Invalid p | Outdated (_, p) ->
        (acc, Delete p)

  let commit __FUN = function
    | Keep _ ->
        Lwt.return_unit
    | Delete p ->
        D.info "%s: Deleting '%s'" __FUN p ;
        Lwt_unix.unlink p
    | Move {from; into} ->
        D.info "%s: Moving '%s' to '%s'" __FUN from into ;
        Lwt_unix.rename from into

  let rec delete_empty_dirs ~delete_root root =
    (* Delete subdirectories, then *)
    let* files = files_in root ~otherwise:(fun _ -> Lwt.return []) in
    let* () =
      Lwt_list.iter_p
        (fun path ->
          let* {st_kind; _} = Lwt_unix.stat path in
          match st_kind with
          | S_DIR ->
              delete_empty_dirs ~delete_root:true path
          | _ ->
              Lwt.return_unit
        )
        files
    in
    if not delete_root then
      Lwt.return_unit
    else
      let* files = files_in root ~otherwise:(fun _ -> Lwt.return []) in
      Lwt.catch
        (fun () ->
          if files = [] then
            Lwt_unix.rmdir root
          else
            Lwt.return_unit
        )
        (fun _ -> Lwt.return_unit)

  (* The code assumes it's the only with access to the disk cache while running *)
  let retime_cache_contents typ =
    let now = Mtime_clock.now () in
    let root = cache_of typ in
    let* contents = get_all_contents root in
    let pending, actions =
      contents |> List.fold_left_map (get_fs_action root now) []
    in
    let* () = Lwt_list.iter_p (commit __FUNCTION__) actions in
    let* () = delete_empty_dirs ~delete_root:false root in
    Lwt.return pending
end

let setup typ read write =
  let* pending = Setup.retime_cache_contents typ in
  let capacity = 512 in
  let queue, push = Lwt_bounded_stream.create capacity in
  let lock = Lwt_mutex.create () in
  let state =
    if pending = [] then
      Direct
    else if List.length pending < capacity then
      let () =
        List.iter (fun e -> Option.value ~default:() (push (Some e))) pending
      in
      Engaged
    else
      Disengaged
  in
  let q = {queue; push; lock; state} in
  Lwt.return
    ( Writer.with_cache ~direct:(read, write) typ q
    , Watcher.watch ~direct:write typ q
    )
