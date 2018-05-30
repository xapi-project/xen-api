(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
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


module Mutex = struct
  include Mutex

  (** execute the function f with the mutex hold *)
  let execute lock f =
    Mutex.lock lock;
    let r = begin try f () with exn -> Mutex.unlock lock; raise exn end; in
    Mutex.unlock lock;
    r
end

module Cache = struct
  type 'a t = {
    mutable item: 'a option;
    fn: unit -> 'a;
    m: Mutex.t;
  }

  let make fn =
    let item = None in
    let m = Mutex.create () in
    { item; fn; m }

  let invalidate t =
    Mutex.execute t.m
      (fun () ->
         t.item <- None;
      )

  let get t =
    Mutex.execute t.m
      (fun () ->
         match t.item with
         | Some x -> x
         | None ->
           let x = t.fn () in
           t.item <- Some x;
           x
      )
end

let hostname = Cache.make
    (fun () ->
       let h = Unix.gethostname () in
       Backtrace.set_my_name (Filename.basename(Sys.argv.(0)) ^ " @ " ^ h);
       h
    )

let invalidate_hostname_cache () = Cache.invalidate hostname

let get_thread_id () =
  try Thread.id (Thread.self ()) with _ -> -1

module ThreadLocalTable = struct
  type 'a t = {
    tbl: (int, 'a) Hashtbl.t;
    m: Mutex.t;
  }

  let make () =
    let tbl = Hashtbl.create 37 in
    let m = Mutex.create () in
    { tbl; m }

  let add t v =
    let id = get_thread_id () in
    Mutex.execute t.m (fun () -> Hashtbl.add t.tbl id v)

  let remove t =
    let id = get_thread_id () in
    Mutex.execute t.m (fun () -> Hashtbl.remove t.tbl id)

  let find t =
    let id = get_thread_id () in
    Mutex.execute t.m (fun () ->
        try
          Some (Hashtbl.find t.tbl id)
        with
        | _ -> None
      )
end

let names = ThreadLocalTable.make ()

let tasks = ThreadLocalTable.make ()

let gettimestring () =
  let time = Unix.gettimeofday () in
  let tm = Unix.gmtime time in
  let msec = time -. (floor time) in
  Printf.sprintf "%d%.2d%.2dT%.2d:%.2d:%.2d.%.3dZ|"
    (1900 + tm.Unix.tm_year)
    (tm.Unix.tm_mon + 1) tm.Unix.tm_mday
    tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec
    (int_of_float (1000.0 *. msec))

let format include_time brand priority message =
  let host = Cache.get hostname in
  let id = get_thread_id () in
  let name = match ThreadLocalTable.find names with Some x -> x | None -> "" in
  let task = match ThreadLocalTable.find tasks with Some x -> x | None -> "" in

  Printf.sprintf "[%s%5s|%s|%d %s|%s|%s] %s"
    (if include_time then gettimestring () else "")
    priority host id name task brand message

let print_debug = ref false
let log_to_stdout () = print_debug := true

let loglevel_m = Mutex.create ()
let logging_disabled_for : (string * Syslog.level, unit) Hashtbl.t = Hashtbl.create 0
let default_loglevel = Syslog.Debug
let loglevel = ref default_loglevel

let disabled_modules () =
  Mutex.execute loglevel_m (fun () ->
      Hashtbl.fold (fun key _ acc -> key :: acc) logging_disabled_for []
    )

let is_disabled brand level =
  Mutex.execute loglevel_m (fun () ->
      Syslog.is_masked ~threshold:!loglevel level ||
      Hashtbl.mem logging_disabled_for (brand, level)
    )

let reset_levels () =
  Mutex.execute loglevel_m (fun () ->
      loglevel := default_loglevel;
      Hashtbl.clear logging_disabled_for
    )


let facility = ref Syslog.Daemon
let facility_m = Mutex.create ()
let set_facility f = Mutex.execute facility_m (fun () -> facility := f)
let get_facility () = Mutex.execute facility_m (fun () -> !facility)

let output_log brand level priority s =
  if not(is_disabled brand level) then begin
    let msg = format false brand priority s in
    if !print_debug
    then Printf.printf "%s\n%!" (format true brand priority s);

    Syslog.log (get_facility ()) level msg
  end

let rec split_c c str =
  try
    let i = String.index str c in
    String.sub str 0 i :: (split_c c (String.sub str (i+1) (String.length str - i - 1)))
  with Not_found -> [str]

let log_backtrace_exn ?(level=Syslog.Err) ?(msg="error") exn _bt =
  Backtrace.is_important exn;
  let all = split_c '\n' (Backtrace.(to_string_hum (remove exn))) in
  (* Write to the log line at a time *)
  output_log "backtrace" level msg (Printf.sprintf "Raised %s" (Printexc.to_string exn));
  List.iter (output_log "backtrace" level msg) all

let log_backtrace e bt = log_backtrace_exn e bt

let with_thread_associated task f x =
  ThreadLocalTable.add tasks task;
  let result = Backtrace.with_backtraces (fun () -> f x) in
  ThreadLocalTable.remove tasks;
  match result with
  | `Ok result ->
    result
  | `Error (exn, bt) ->
    (* This function is a top-level exception handler typically used on fresh
       threads. This is the last chance to do something with the backtrace *)
    output_log "backtrace" Syslog.Err "error" (Printf.sprintf "%s failed with exception %s" task (Printexc.to_string exn));
    log_backtrace exn bt;
    raise exn

let with_thread_named name f x =
  ThreadLocalTable.add names name;
  try
    let result = f x in
    ThreadLocalTable.remove names;
    result
  with e ->
    Backtrace.is_important e;
    ThreadLocalTable.remove names;
    raise e

module StringSet = Set.Make(struct type t=string let compare=Pervasives.compare end)
let debug_keys = ref StringSet.empty
let get_all_debug_keys () =
  StringSet.fold (fun key keys -> key::keys) !debug_keys []

let dkmutex = Mutex.create ()

module type BRAND = sig
  val name: string
end

let all_levels = [Syslog.Debug; Syslog.Info; Syslog.Warning; Syslog.Err]

let add_to_stoplist brand level =
  Hashtbl.replace logging_disabled_for (brand, level) ()

let remove_from_stoplist brand level =
  Hashtbl.remove logging_disabled_for (brand, level)

let disable ?level brand =
  let levels = match level with
    | None -> all_levels
    | Some l -> [l]
  in
  Mutex.execute loglevel_m (fun () ->
      List.iter (add_to_stoplist brand) levels
    )

let enable ?level brand =
  let levels = match level with
    | None -> all_levels
    | Some l -> [l]
  in
  Mutex.execute loglevel_m (fun () ->
      List.iter (remove_from_stoplist brand) levels
    )

let set_level level =
  Mutex.execute loglevel_m (fun () ->
      loglevel := level
    )

module type DEBUG = sig
  val debug : ('a, unit, string, unit) format4 -> 'a

  val warn : ('a, unit, string, unit) format4 -> 'a

  val info : ('a, unit, string, unit) format4 -> 'a

  val error : ('a, unit, string, unit) format4 -> 'a

  val audit : ?raw:bool -> ('a, unit, string, string) format4 -> 'a

  val log_backtrace : unit -> unit

  val log_and_ignore_exn : (unit -> unit) -> unit
end

module Make = functor(Brand: BRAND) -> struct
  let _ =
    Mutex.execute dkmutex (fun () ->
        debug_keys := StringSet.add Brand.name !debug_keys)

  let output level priority (fmt: ('a, unit, string, 'b) format4) =
    Printf.kprintf
      (fun s ->
         if not(is_disabled Brand.name level)
         then output_log Brand.name level priority s
      ) fmt

  let debug fmt = output Syslog.Debug "debug" fmt
  let warn fmt = output Syslog.Warning "warn" fmt
  let info fmt = output Syslog.Info "info" fmt
  let error fmt = output Syslog.Err "error" fmt
  let audit ?(raw=false) (fmt: ('a, unit, string, 'b) format4) =
    Printf.kprintf
      (fun s ->
         let msg = if raw then s else format true Brand.name "audit" s in
         Syslog.log Syslog.Local6 Syslog.Info msg;
         msg
      ) fmt

  let log_backtrace () =
    let backtrace = Printexc.get_backtrace () in
    debug "%s" (String.escaped backtrace)

  let log_and_ignore_exn f =
    try f ()
    with e -> log_backtrace_exn ~level:Syslog.Debug ~msg:"debug" e ()
end

