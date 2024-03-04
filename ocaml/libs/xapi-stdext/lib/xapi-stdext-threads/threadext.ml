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

module Mutex = struct
  (** execute the function f with the mutex hold *)
  let execute lock f =
    Mutex.lock lock ;
    Xapi_stdext_pervasives.Pervasiveext.finally f (fun () -> Mutex.unlock lock)
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
  (* Concrete type is the ends of a pipe *)
  type t = {
      (* A pipe is used to wake up a thread blocked in wait: *)
      mutable pipe_out: Unix.file_descr option
    ; mutable pipe_in: Unix.file_descr option
    ; (* Indicates that a signal arrived before a wait: *)
      mutable signalled: bool
    ; m: M.t
  }

  let make () =
    {pipe_out= None; pipe_in= None; signalled= false; m= M.create ()}

  exception Pre_signalled

  let wait (x : t) (seconds : float) =
    let finally = Xapi_stdext_pervasives.Pervasiveext.finally in
    let to_close = ref [] in
    let close' fd =
      if List.mem fd !to_close then Unix.close fd ;
      to_close := List.filter (fun x -> fd <> x) !to_close
    in
    finally
      (fun () ->
        try
          let pipe_out =
            Mutex.execute x.m (fun () ->
                if x.signalled then (
                  x.signalled <- false ;
                  raise Pre_signalled
                ) ;
                let pipe_out, pipe_in = Unix.pipe () in
                (* these will be unconditionally closed on exit *)
                to_close := [pipe_out; pipe_in] ;
                x.pipe_out <- Some pipe_out ;
                x.pipe_in <- Some pipe_in ;
                x.signalled <- false ;
                pipe_out
            )
          in
          let open Xapi_stdext_unix.Unixext in
          (* flush the single byte from the pipe *)
          try
            let (_ : string) =
              time_limited_single_read pipe_out 1 ~max_wait:seconds
            in
            false
          with Timeout -> true
          (* return true if we waited the full length of time, false if we were woken *)
        with Pre_signalled -> false
      )
      (fun () ->
        Mutex.execute x.m (fun () ->
            x.pipe_out <- None ;
            x.pipe_in <- None ;
            List.iter close' !to_close
        )
      )

  let signal (x : t) =
    Mutex.execute x.m (fun () ->
        match x.pipe_in with
        | Some fd ->
            ignore (Unix.write fd (Bytes.of_string "X") 0 1)
        | None ->
            x.signalled <- true
        (* If the wait hasn't happened yet then store up the signal *)
    )
end
