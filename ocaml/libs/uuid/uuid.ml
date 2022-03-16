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
        Bytes.to_string buf
    )
    (fun () -> Unix.close fd)

let make_uuid_urnd () = of_bytes (read_bytes dev_urandom 16) |> Option.get

(* Use the CSPRNG-backed urandom *)
let make = make_uuid_urnd


type cookie = string

let make_cookie () =
  read_bytes dev_urandom 64
  |> String.to_seq
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
