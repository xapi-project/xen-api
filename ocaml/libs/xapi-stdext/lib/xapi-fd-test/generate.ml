(*
 * Copyright (C) 2023 Cloud Software Group
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

open Xapi_fdcaps
open Operations
open Observations

type t = {
    size: int
  ; delay_read: Delay.t option
  ; delay_write: Delay.t option
  ; kind: Unix.file_kind
}

let make ~size ~delay_read ~delay_write kind =
  {size; delay_read; delay_write; kind}

open QCheck2

let all_file_kinds = Unix.[S_BLK; S_CHR; S_DIR; S_FIFO; S_LNK; S_REG; S_SOCK]

let file_kind =
  (* [Gen.oneofa] should be more efficient than [Gen.oneofl] *)
  ( all_file_kinds |> Array.of_list |> Gen.oneofa
  , Print.contramap (Fmt.to_to_string Safefd.pp_kind) Print.string
  )

let is_testable_kind = function
  | Unix.(S_DIR | S_LNK) ->
      (* unless you write a custom C binding, you cannot open these in OCaml *)
      false
  | Unix.S_BLK ->
      Unix.geteuid () = 0
  | Unix.(S_CHR | S_FIFO | S_REG | S_SOCK) ->
      (* We cannot create new [S_CHR], but there are preexisting [S_CHR],
         like [/dev/null]. *)
      true

let testable_file_kind =
  ( all_file_kinds |> List.filter is_testable_kind |> Array.of_list |> Gen.oneofa
  , snd file_kind
  )

(* also coincidentally the pipe buffer size on Linux *)
let ocaml_unix_buffer_size = 65536

let sizes =
  Gen.oneofa
    [|
       0
     ; 1
     ; 100
     ; 4096
     ; ocaml_unix_buffer_size - 1
     ; ocaml_unix_buffer_size
     ; ocaml_unix_buffer_size + 1
     ; 2 * ocaml_unix_buffer_size
     ; (10 * ocaml_unix_buffer_size) + 3
    |]

(* some may exceed length of test, but that is what the timeout is for *)
let total_delays = Gen.oneofa [|0.001; 0.01; 0.1; 0.4|]

let span_of_s s = s *. 1e9 |> Mtime.Span.of_float_ns |> Option.get

(* keep these short *)
let timeouts = Gen.oneofa [|0.0; 0.001; 0.1; 0.3|]

let delay_of_size total_delay size =
  let open Gen in
  let* every_bytes = if size = 0 then return 1 else 1 -- size in
  let chunks = max 1 (size / every_bytes) in
  let duration = total_delay /. float_of_int chunks in
  if duration < 1e-6 then
    return None
  else
    let duration = duration |> span_of_s in
    return @@ Some (Delay.v ~every_bytes ~duration)

let t =
  let open Gen in
  (* order matters here for shrinking: shrink timeout first so that shrinking completes sooner! *)
  let* total_delay = total_delays
  and* size = sizes
  and* kind = fst testable_file_kind in
  let size = if kind = Unix.S_BLK then 512 else size in
  let* delay = delay_of_size total_delay size in
  (* see observations.ml, we can't easily change size afterwards *)
  return @@ make ~delay_read:delay ~delay_write:delay ~size kind

let print t =
  (* to easily grep print on single line *)
  let buf = Buffer.create 128 in
  let fmt = Fmt.with_buffer buf in
  Format.pp_set_geometry fmt ~max_indent:999 ~margin:1000 ;
  Fmt.(
    record ~sep:(any "; ")
      [
        field "delay_read" (fun t -> t.delay_read) (option Delay.pp)
      ; field "delay_write" (fun t -> t.delay_write) (option Delay.pp)
      ; field "size" (fun t -> t.size) int
      ; field "file_kind" (fun t -> (snd file_kind) t.kind) string
      ]
  )
    fmt t ;
  Fmt.flush fmt () ;
  Buffer.contents buf

let run_ro t data ~f =
  (* we can only implement delays on write, skip *)
  CancellableSleep.with_ @@ fun cancel ->
  let finally () = CancellableSleep.cancel cancel in
  let f arg = Fun.protect ~finally (fun () -> f arg) in
  let write =
    match t.delay_write with
    | Some delay ->
        Delay.apply_write cancel delay single_write_substring
    | None ->
        single_write_substring
  in
  observe_ro write ~f t.kind data

let run_wo t ~f =
  CancellableSleep.with_ @@ fun cancel ->
  let finally () = CancellableSleep.cancel cancel in
  let f arg = Fun.protect ~finally (fun () -> f arg) in
  let read =
    match t.delay_read with
    | Some delay ->
        Delay.apply_read cancel delay read
    | None ->
        read
  in
  observe_wo read ~f t.kind ~size:t.size

let run_rw t data ~f =
  CancellableSleep.with_ @@ fun cancel ->
  let finally () = CancellableSleep.cancel cancel in
  let f arg = Fun.protect ~finally (fun () -> f arg) in
  let read =
    match t.delay_read with
    | Some delay ->
        Delay.apply_read cancel delay read
    | None ->
        read
  in
  let write =
    match t.delay_write with
    | Some delay ->
        Delay.apply_write cancel delay single_write_substring
    | None ->
        single_write_substring
  in
  observe_rw read write ~f t.kind ~size:t.size data
