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

module ThreadRuntimeContext = struct
  type t = {
      ocaml_tid: int
    ; thread_name: string
    ; mutable time_running: Mtime.span
    ; mutable tepoch: int
    ; tgroup: Tgroup.Group.t
  }

  (*The documentation for Ambient_context_thread_local isn't really clear is
    this context. thread_local_storage is a global variable shared by all
    threads. It is a map with keys, the thread IDs and values the above
    defined data structure.*)
  let thread_local_storage = Ambient_context_thread_local.Thread_local.create ()

  let create ?(thread_name = "") () =
    let ocaml_tid = Thread.self () |> Thread.id in
    let time_running = Mtime.Span.zero in
    let tepoch = 0 in
    let tgroup =
      Tgroup.Group.(
        of_creator (Creator.make ~identity:Identity.root_identity ())
      )
    in
    let tls = {thread_name; tgroup; ocaml_tid; time_running; tepoch} in
    let () =
      Ambient_context_thread_local.Thread_local.set thread_local_storage tls
    in
    tls

  let get () =
    Ambient_context_thread_local.Thread_local.get_or_create ~create
      thread_local_storage

  let update f context =
    f context
    |> Ambient_context_thread_local.Thread_local.set thread_local_storage

  let remove () =
    Ambient_context_thread_local.Thread_local.remove thread_local_storage
end
