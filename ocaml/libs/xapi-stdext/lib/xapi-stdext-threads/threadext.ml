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

module M = Mutex

let finally = Xapi_stdext_pervasives.Pervasiveext.finally

module Mutex = struct
  (** execute the function f with the mutex hold *)
  let execute lock f =
    Mutex.lock lock ;
    finally f (fun () -> Mutex.unlock lock)
end

module Semaphore = struct
  let execute s f =
    let module Semaphore = Semaphore.Counting in
    Semaphore.acquire s ;
    finally f (fun () -> Semaphore.release s)
end

(** Parallel List.iter. Remembers all exceptions and returns an association list mapping input x to an exception.
    Applications of x which succeed will be missing from the returned list. *)
let thread_iter_all_exns f xs =
  let exns = ref [] in
  let m = M.create () in
  List.iter Thread.join
    (List.map
       (fun x ->
         Thread.create
           (fun () ->
             try f x
             with e -> Mutex.execute m (fun () -> exns := (x, e) :: !exns)
           )
           ()
       )
       xs
    ) ;
  !exns

(** Parallel List.iter. Remembers one exception (at random) and throws it in the
    error case. *)
let thread_iter f xs =
  match thread_iter_all_exns f xs with [] -> () | (_, e) :: _ -> raise e

module Delay = struct
  type t

  external make : unit -> t = "caml_xapi_delay_create"

  external signal : t -> unit = "caml_xapi_delay_signal"

  external wait : t -> int64 -> bool = "caml_xapi_delay_wait"

  let wait d t =
    if t <= 0. then
      true
    else
      match Mtime.Span.of_float_ns (t *. 1e9) with
      | Some span ->
          let now = Mtime_clock.now () in
          let deadline =
            Mtime.add_span now span |> Option.value ~default:Mtime.max_stamp
          in
          wait d (Mtime.to_uint64_ns deadline)
      | None ->
          invalid_arg "Time specified too big"
end

let wait_timed_read fd timeout =
  match Xapi_stdext_unix.Unixext.select [fd] [] [] timeout with
  | [], _, _ ->
      false
  | [fd'], _, _ ->
      assert (fd' = fd) ;
      true
  | _ ->
      assert false

let wait_timed_write fd timeout =
  match Xapi_stdext_unix.Unixext.select [] [fd] [] timeout with
  | _, [], _ ->
      false
  | _, [fd'], _ ->
      assert (fd' = fd) ;
      true
  | _ ->
      assert false

module ThreadLocalStorage = struct
  type t = {
      ocaml_tid: int
    ; thread_name: string
    ; time_running: Mtime.span
    ; time_last_yield: Mtime_clock.counter
    ; tepoch: Mtime.t
    ; tgroup: Tgroup.Group.t
  }

  let thread_local_storage = Ambient_context_thread_local.Thread_local.create ()

  let create () =
    let ocaml_tid = Thread.self () |> Thread.id in
    let thread_name = "" in
    let time_running = Mtime.Span.zero in
    let time_last_yield = Mtime_clock.counter () in
    let tepoch = Mtime_clock.now () in
    let tgroup =
      Tgroup.Group.(
        of_creator (Creator.make ~identity:Identity.root_identity ())
      )
    in
    let tls =
      {thread_name; tgroup; ocaml_tid; time_running; time_last_yield; tepoch}
    in
    let () =
      Ambient_context_thread_local.Thread_local.set thread_local_storage tls
    in
    tls

  let get () =
    Ambient_context_thread_local.Thread_local.get_or_create ~create
      thread_local_storage

  let set ?thread_name ?time_running ?time_last_yield ?tepoch ?tgroup () =
    let tls = get () in
    let tls =
      Option.fold ~none:tls
        ~some:(fun thread_name -> {tls with thread_name})
        thread_name
    in
    let tls =
      Option.fold ~none:tls
        ~some:(fun time_running -> {tls with time_running})
        time_running
    in
    let tls =
      Option.fold ~none:tls
        ~some:(fun time_last_yield -> {tls with time_last_yield})
        time_last_yield
    in
    let tls =
      Option.fold ~none:tls ~some:(fun tepoch -> {tls with tepoch}) tepoch
    in
    let tls =
      Option.fold ~none:tls ~some:(fun tgroup -> {tls with tgroup}) tgroup
    in
    Ambient_context_thread_local.Thread_local.set thread_local_storage tls

  let remove () =
    Ambient_context_thread_local.Thread_local.remove thread_local_storage
end
