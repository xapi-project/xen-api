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

type 'a t = Uuidm.t

let null = Uuidm.nil

let pp = Uuidm.pp

let equal = Uuidm.equal

let of_bytes u = Uuidm.of_binary_string ~pos:0 u

let to_bytes = Uuidm.to_binary_string

let of_int_array arr =
  arr |> Array.to_seq |> Seq.map char_of_int |> String.of_seq |> of_bytes

let to_int_array u =
  to_bytes u |> String.to_seq |> Seq.map int_of_char |> Array.of_seq

let of_string = Uuidm.of_string ~pos:0

let to_string = Uuidm.to_string ~upper:false

let is_uuid str = match of_string str with None -> false | Some _ -> true

let dev_urandom = "/dev/urandom"

let read_bytes dev n =
  let fd = Unix.openfile dev [Unix.O_RDONLY] 0o640 in
  let finally body_f clean_f =
    try
      let ret = body_f () in
      clean_f () ; ret
    with e -> clean_f () ; raise e
  in
  finally
    (fun () ->
      let buf = Bytes.create n in
      let read = Unix.read fd buf 0 n in
      if read <> n then
        raise End_of_file
      else
        buf
    )
    (fun () -> Unix.close fd)

let rand64 () =
  let b = read_bytes dev_urandom 8 in
  Bytes.get_int64_ne b 0

let make_uuid_urnd () =
  let buf = read_bytes dev_urandom 16 in
  Uuidm.v4 buf

let make_v7_uuid_from_parts time_ns rand_b = Uuidm.v7_ns ~time_ns ~rand_b

let now_ns =
  let start = Mtime_clock.counter () in
  let t0 =
    let d, ps = Ptime_clock.now () |> Ptime.to_span |> Ptime.Span.to_d_ps in
    Int64.(add (mul (of_int d) 86_400_000_000_000L) (div ps 1000L))
  in
  fun () ->
    let since_t0 = Mtime_clock.count start |> Mtime.Span.to_uint64_ns in
    Int64.add t0 since_t0

let make_v7_uuid () = make_v7_uuid_from_parts (now_ns ()) (rand64 ())

(* Use the CSPRNG-backed urandom *)
let make = make_uuid_urnd

type cookie = string

let make_cookie () =
  read_bytes dev_urandom 64
  |> Bytes.to_seq
  |> Seq.map (fun c -> Printf.sprintf "%1x" (int_of_char c))
  |> List.of_seq
  |> String.concat ""

let string_of_cookie s = s

let cookie_of_string s = s

(* deprecated: we don't need to duplicate the uuid prefix/suffix *)
let uuid_of_string = of_string

let string_of_uuid = to_string

let uuid_of_int_array = of_int_array

let int_array_of_uuid = to_int_array

module Hash = struct
  (** Derive a deterministic UUID from a string: the same
      string maps to the same UUID. We are using our own namespace; the
      namespace is not a secret *)

  let namespace =
    let ns = "e93e0639-2bdb-4a59-8b46-352b3f408c19" in
    Uuidm.(of_string ns |> Option.get)

  let string str = Uuidm.v5 namespace str
end
