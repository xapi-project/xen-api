(*
 * Copyright (c) Cloud Software Group, Inc.
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

module D = Debug.Make (struct let name = "io_watchdog" end)

let with_lock = Xapi_stdext_threads.Threadext.Mutex.execute

let read path =
  try Some (Xapi_stdext_unix.Unixext.string_of_file path) with _ -> None

let io_of pid =
  match read (Printf.sprintf "/proc/%d/io" pid) with
  | None ->
      0L
  | Some io ->
      String.split_on_char '\n' io
      |> List.fold_left
           (fun acc line ->
             match Astring.String.cut ~sep:": " line with
             | Some (("rchar" | "wchar"), n) ->
                 Int64.(add acc (of_string_opt n |> Option.value ~default:0L))
             | _ ->
                 acc
           )
           0L

let children pid =
  let tasks = Printf.sprintf "/proc/%d/task" pid in
  (try Sys.readdir tasks with _ -> [||])
  |> Array.to_list
  |> List.concat_map (fun tid ->
      read (Filename.concat tasks tid ^ "/children")
      |> Option.fold ~none:[] ~some:(String.split_on_char ' ')
      |> List.filter_map int_of_string_opt
  )

(* A child that has exited and been reaped has its counts folded into its
   parent's, so the sum does not go backwards when one goes away. *)
let activity pid =
  List.fold_left (fun acc p -> Int64.add acc (io_of p)) 0L (pid :: children pid)

let with_watchdog ~name ~pid ~timeout ~abort f =
  let duration =
    Clock.Timer.s_to_span timeout |> Option.value ~default:Mtime.Span.max_span
  in
  let interval = Float.(max 0.1 (min 5. timeout)) in
  let m = Mutex.create () in
  let finished = ref false in
  let fired = ref false in
  (* Not joined, so that [f] returns as soon as it is done. [abort] runs under
     [m], and [finished] is set under [m] before we return, so the caller can
     release whatever [abort] uses as soon as we do. *)
  let watchdog () =
    let rec wait seen timer =
      Thread.delay interval ;
      let next =
        with_lock m (fun () ->
            if !finished then
              None
            else
              let now = activity pid in
              if now <> seen then
                Some (now, Clock.Timer.start ~duration)
              else if Clock.Timer.has_expired timer then (
                D.debug "%s pid %d made no progress for %.0fs; aborting" name
                  pid timeout ;
                fired := true ;
                (try abort () with _ -> ()) ;
                None
              ) else
                Some (seen, timer)
        )
      in
      Option.iter (fun (seen, timer) -> wait seen timer) next
    in
    wait (activity pid) (Clock.Timer.start ~duration)
  in
  let (_ : Thread.t) = Thread.create watchdog () in
  Fun.protect
    ~finally:(fun () -> with_lock m (fun () -> finished := true))
    (fun () ->
      try f ()
      with e when with_lock m (fun () -> !fired) ->
        D.error "%s pid %d was aborted after %.0fs without progress: %s" name
          pid timeout (Printexc.to_string e) ;
        let msg =
          Printf.sprintf "%s made no progress for %.0fs and was aborted" name
            timeout
        in
        raise Xenops_interface.(Xenopsd_error (Errors.Internal_error msg))
    )
