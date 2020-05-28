(*
 * Copyright (C) Citrix Systems Inc.
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

open Xapi_stdext_pervasives
open Xapi_stdext_threads

module D = Debug.Make (struct let name = "scheduler" end)

open D

module Int64Map = Map.Make (struct
  type t = int64

  let compare = Int64.compare
end)

module Delay = struct
  (* Concrete type is the ends of a pipe *)
  type t = {
      (* A pipe is used to wake up a thread blocked in wait: *)
      mutable pipe_out: Unix.file_descr option
    ; mutable pipe_in: Unix.file_descr option
    ; (* Indicates that a signal arrived before a wait: *)
      mutable signalled: bool
    ; m: Mutex.t
  }

  let make () =
    {pipe_out= None; pipe_in= None; signalled= false; m= Mutex.create ()}

  exception Pre_signalled

  let wait (x : t) (seconds : float) =
    let timeout = if seconds < 0.0 then 0.0 else seconds in
    let to_close = ref [] in
    let close' fd =
      if List.mem fd !to_close then Unix.close fd ;
      to_close := List.filter (fun x -> fd <> x) !to_close
    in
    Pervasiveext.finally
      (fun () ->
        try
          let pipe_out =
            Threadext.Mutex.execute x.m (fun () ->
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
                pipe_out)
          in
          let r, _, _ = Unix.select [pipe_out] [] [] timeout in
          (* flush the single byte from the pipe *)
          if r <> [] then ignore (Unix.read pipe_out (Bytes.create 1) 0 1) ;
          (* return true if we waited the full length of time, false if we were
             woken *)
          r = []
        with Pre_signalled -> false)
      (fun () ->
        Threadext.Mutex.execute x.m (fun () ->
            x.pipe_out <- None ;
            x.pipe_in <- None ;
            List.iter close' !to_close))

  let signal (x : t) =
    Threadext.Mutex.execute x.m (fun () ->
        match x.pipe_in with
        | Some fd ->
            ignore (Unix.write fd (Bytes.of_string "X") 0 1)
        | None ->
            x.signalled <- true
        (* If the wait hasn't happened yet then store up the signal *))
end

type item = {id: int; name: string; fn: unit -> unit}

type handle = int64 * int [@@deriving rpc]

type t = {
    mutable schedule: item list Int64Map.t
  ; delay: Delay.t
  ; mutable next_id: int
  ; m: Mutex.t
}

type time = Delta of int

(*type t = int64 * int [@@deriving rpc]*)

let now () = Unix.gettimeofday () |> ceil |> Int64.of_float

module Dump = struct
  type u = {time: int64; thing: string} [@@deriving rpc]

  type dump = u list [@@deriving rpc]

  let make s =
    let now = now () in
    Threadext.Mutex.execute s.m (fun () ->
        Int64Map.fold
          (fun time xs acc ->
            List.map (fun i -> {time= Int64.sub time now; thing= i.name}) xs
            @ acc)
          s.schedule [])
end

let one_shot s (Delta x) (name : string) f =
  let time = Int64.(add (of_int x) (now ())) in
  let id =
    Threadext.Mutex.execute s.m (fun () ->
        let existing = try Int64Map.find time s.schedule with _ -> [] in
        let id = s.next_id in
        s.next_id <- s.next_id + 1 ;
        let item = {id; name; fn= f} in
        s.schedule <- Int64Map.add time (item :: existing) s.schedule ;
        Delay.signal s.delay ;
        id)
  in
  (time, id)

let cancel s (time, id) =
  Threadext.Mutex.execute s.m (fun () ->
      let existing =
        if Int64Map.mem time s.schedule then
          Int64Map.find time s.schedule
        else
          []
      in
      s.schedule <-
        Int64Map.add time
          (List.filter (fun i -> i.id <> id) existing)
          s.schedule)

let process_expired s =
  let t = now () in
  let expired =
    Threadext.Mutex.execute s.m (fun () ->
        let lt, eq, unexpired = Int64Map.split t s.schedule in
        let expired =
          match eq with None -> lt | Some eq -> Int64Map.add t eq lt
        in
        s.schedule <- unexpired ;
        Int64Map.fold (fun _ stuff acc -> acc @ stuff) expired [] |> List.rev)
  in
  (* This might take a while *)
  List.iter
    (fun i ->
      try i.fn ()
      with e ->
        debug "Scheduler ignoring exception: %s\n%!" (Printexc.to_string e))
    expired ;
  expired <> []

(* true if work was done *)

let rec main_loop s =
  while process_expired s do
    ()
  done ;
  let sleep_until =
    Threadext.Mutex.execute s.m (fun () ->
        try Int64Map.min_binding s.schedule |> fst
        with Not_found -> Int64.add 3600L (now ()))
  in
  let seconds = Int64.sub sleep_until (now ()) in
  let (_ : bool) = Delay.wait s.delay (Int64.to_float seconds) in
  main_loop s

let make () =
  let s =
    {
      schedule= Int64Map.empty
    ; delay= Delay.make ()
    ; next_id= 0
    ; m= Mutex.create ()
    }
  in
  let (_ : Thread.t) = Thread.create main_loop s in
  s
