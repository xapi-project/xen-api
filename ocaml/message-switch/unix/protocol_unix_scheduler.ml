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

let with_lock = Xapi_stdext_threads.Threadext.Mutex.execute

module Int64Map = Map.Make (Int64)
module Delay = Xapi_stdext_threads.Threadext.Delay

type item = {id: int; fn: unit -> unit}

let schedule = ref Int64Map.empty

let delay = Delay.make ()

let next_id = ref 0

let m = Mutex.create ()

let now () = Unix.gettimeofday () |> ceil |> Int64.of_float

let run_after ~seconds f =
  let time = Int64.(add (of_int seconds) (now ())) in
  let id =
    with_lock m (fun () ->
        let existing =
          if Int64Map.mem time !schedule then
            Int64Map.find time !schedule
          else
            []
        in
        let id = !next_id in
        incr next_id ;
        let item = {id; fn= f} in
        schedule := Int64Map.add time (item :: existing) !schedule ;
        Delay.signal delay ;
        id
    )
  in
  (time, id)

let cancel (time, id) =
  with_lock m (fun () ->
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
    with_lock m (fun () ->
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
    with_lock m (fun () ->
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
