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

let of_bytes u = Uuidm.of_bytes ~pos:0 u

let to_bytes = Uuidm.to_bytes

let of_int_array arr =
  arr |> Array.to_seq |> Seq.map char_of_int |> String.of_seq |> of_bytes

let to_int_array u =
  Uuidm.to_bytes u |> String.to_seq |> Seq.map int_of_char |> Array.of_seq

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

let set_bits buf i value mask =
  let old_octet = Bytes.get_uint8 buf i in
  let new_octet = lnot mask land old_octet lor (mask land value) in
  Bytes.set_uint8 buf i new_octet

let set_var_and_ver ver buf =
  (* Sets the 2-bit variant field and the 4-bit version field (see RFC 9562
     sections 4.1 and 4.2 respectively) into a 16 byte buffer representing a
     UUID. *)
  set_bits buf 8 (2 lsl 6) (0x3 lsl 6) ;
  set_bits buf 6 (ver lsl 4) (0xf lsl 4) ;
  ()

let of_buf buf = of_bytes (buf |> Bytes.to_string) |> Option.get

let make_uuid_urnd () =
  let buf = read_bytes dev_urandom 16 in
  set_var_and_ver 4 buf ; of_buf buf

let make_v4_uuid upper lower =
  let buf = Bytes.create 16 in
  Bytes.set_int64_be buf 0 upper ;
  Bytes.set_int64_be buf 8 lower ;
  set_var_and_ver 4 buf ;
  of_buf buf

let d_ps_ns_to_ms_ps =
  let ms_in_day = 86_400_000L in
  let ps_in_ms = 1_000_000_000L in
  let ps_in_ns = 1_000L in
  let ( + ) = Int64.add in
  let ( * ) = Int64.mul in
  let ( / ) = Int64.div in
  let ( mod ) = Int64.rem in
  fun days ps ns_adj ->
    let ps_total = ps + (ps_in_ns * ns_adj) in
    let ms = (ms_in_day * Int64.of_int days) + (ps_total / ps_in_ms) in
    let ps = ps_total mod ps_in_ms in
    (ms, ps)

let make_v7_uuid_from_parts =
  let ( * ) = Int64.mul in
  let ( lsl ) = Int64.shift_left in
  let ( lsr ) = Int64.shift_right_logical in
  fun (days, picos) nano_adjustment rand_b ->
    let buf = Bytes.create 16 in
    let ms, ps = d_ps_ns_to_ms_ps days picos nano_adjustment in
    (* We are using 12 bits to contain a sub-millisecond fraction, so we want
       to converted the remaindered number of picoseconds into the range 0 to
       4096. Given there are 10^9 picoseconds in a millisecond, multiplying by
       (2^63 / 10^9) converts into the range 0 - 2^63-1, which we can shift to
       give the 12-bit value we want. *)
    let sub_ms_frac = (ps * 9_223_372_037L) lsr 51 |> Int64.to_int in
    Bytes.set_int64_be buf 0 (ms lsl 16) ;
    Bytes.set_int16_be buf 6 sub_ms_frac ;
    Bytes.set_int64_be buf 8 rand_b ;
    set_var_and_ver 7 buf ;
    of_buf buf

let make_rand64 () =
  let buf = read_bytes dev_urandom 8 in
  Bytes.get_int64_ne buf 0

let make_v7_uuid =
  let start = Mtime_clock.counter () in
  let t0 = Ptime_clock.now () |> Ptime.to_span |> Ptime.Span.to_d_ps in
  fun () ->
    let ns_since_t0 = Mtime_clock.count start |> Mtime.Span.to_uint64_ns in
    make_v7_uuid_from_parts t0 ns_since_t0 (make_rand64 ())

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
