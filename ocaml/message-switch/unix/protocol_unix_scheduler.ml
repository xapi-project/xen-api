(*
 * Copyright (c) Citrix Systems Inc.
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

let finally' f g =
  try
    let result = f () in
    g () ; result
  with e -> g () ; raise e

module Mutex = struct
  include Mutex

  let execute m f =
    lock m ;
    finally' f (fun () -> unlock m)
end

module Int64Map = Map.Make (struct
  type t = int64

  let compare = compare
end)

module Delay = struct
  (* Concrete type is the ends of a pipe *)
  type t = {
      (* A pipe is used to wake up a thread blocked in wait: *)
      mutable sock_out: Unix.file_descr option
    ; mutable sock_in: Unix.file_descr option
    ; (* Indicates that a signal arrived before a wait: *)
      mutable signalled: bool
    ; m: Mutex.t
  }

  let make () =
    {sock_out= None; sock_in= None; signalled= false; m= Mutex.create ()}

  exception Pre_signalled

  let wait (x : t) (seconds : float) =
    let to_close = ref [] in
    let close' fd =
      if List.mem fd !to_close then Unix.close fd ;
      to_close := List.filter (fun x -> fd <> x) !to_close
    in
    finally'
      (fun () ->
        try
          let sock_out =
            Mutex.execute x.m (fun () ->
                if x.signalled then (
                  x.signalled <- false ;
                  raise Pre_signalled
                ) ;
                let sock_out, sock_in =
                  Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0
                in
                (* these will be unconditionally closed on exit *)
                to_close := [sock_out; sock_in] ;
                x.sock_out <- Some sock_out ;
                x.sock_in <- Some sock_in ;
                x.signalled <- false ;
                sock_out
            )
          in
          (* flush the single byte from the socket *)
          Unix.setsockopt_float sock_out Unix.SO_RCVTIMEO seconds ;
          (* return true if we waited the full length of time, false if we were woken *)
          try
            ignore (Unix.read sock_out (Bytes.create 1) 0 1) ;
            Unix.setsockopt_float sock_out Unix.SO_RCVTIMEO 0. ;
            false
          with Unix.Unix_error (Unix.EAGAIN, _, _) -> true
        with Pre_signalled -> false
      )
      (fun () ->
        Mutex.execute x.m (fun () ->
            x.sock_out <- None ;
            x.sock_in <- None ;
            List.iter close' !to_close
        )
      )

  let signal (x : t) =
    Mutex.execute x.m (fun () ->
        match x.sock_in with
        | Some fd ->
            ignore (Unix.write fd (Bytes.of_string "X") 0 1)
        | None ->
            x.signalled <- true
        (* If the wait hasn't happened yet then store up the signal *)
    )
end

type item = {id: int; name: string; fn: unit -> unit}

let schedule = ref Int64Map.empty

let delay = Delay.make ()

let next_id = ref 0

let m = Mutex.create ()

type time = Absolute of int64 | Delta of int [@@deriving rpc]

type t = int64 * int [@@deriving rpc]

let now () = Unix.gettimeofday () |> ceil |> Int64.of_float

module Dump = struct
  type u = {time: int64; thing: string} [@@deriving rpc]

  type t = u list [@@deriving rpc]

  let make () =
    let now = now () in
    Mutex.execute m (fun () ->
        Int64Map.fold
          (fun time xs acc ->
            List.map (fun i -> {time= Int64.sub time now; thing= i.name}) xs
            @ acc
          )
          !schedule []
    )
end

let one_shot time (name : string) f =
  let time =
    match time with
    | Absolute x ->
        x
    | Delta x ->
        Int64.(add (of_int x) (now ()))
  in
  let id =
    Mutex.execute m (fun () ->
        let existing =
          if Int64Map.mem time !schedule then
            Int64Map.find time !schedule
          else
            []
        in
        let id = !next_id in
        incr next_id ;
        let item = {id; name; fn= f} in
        schedule := Int64Map.add time (item :: existing) !schedule ;
        Delay.signal delay ;
        id
    )
  in
  (time, id)

let cancel (time, id) =
  Mutex.execute m (fun () ->
      let existing =
        if Int64Map.mem time !schedule then
          Int64Map.find time !schedule
        else
          []
      in
      schedule :=
        Int64Map.add time (List.filter (fun i -> i.id <> id) existing) !schedule
  )

let process_expired () =
  let t = now () in
  let expired =
    Mutex.execute m (fun () ->
        let expired, unexpired =
          Int64Map.partition (fun t' _ -> t' <= t) !schedule
        in
        schedule := unexpired ;
        Int64Map.fold (fun _ stuff acc -> acc @ stuff) expired [] |> List.rev
    )
  in
  (* This might take a while *)
  List.iter (fun i -> try i.fn () with _e -> ()) expired ;
  expired <> []

(* true if work was done *)

let rec main_loop () =
  while process_expired () do
    ()
  done ;
  let sleep_until =
    Mutex.execute m (fun () ->
        try Int64Map.min_binding !schedule |> fst
        with Not_found -> Int64.add 3600L (now ())
    )
  in
  let seconds = Int64.sub sleep_until (now ()) in
  let (_ : bool) = Delay.wait delay (Int64.to_float seconds) in
  main_loop ()

let start =
  let t = ref None in
  fun () ->
    match !t with
    | None ->
        t := Some (Thread.create main_loop ())
    | Some _ ->
        ()
