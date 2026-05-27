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

(* Websockets protocol here *)

module Wsprotocol (IO : Iteratees.Monad) = struct
  module I = Iteratees.Iteratee (IO)
  open I

  type 'a t = 'a I.t

  let sanitize s =
    (* ignore control characters: see RFC4648.1 and RFC4648.3
     * https://tools.ietf.org/html/rfc4648#section-3
     * Note: \t = \009, \n = \012, \r = \015, \s = \032 *)
    let result = Buffer.create (String.length s) in
    for i = 0 to String.length s - 1 do
      if
        (String.unsafe_get s i >= '\000' && String.unsafe_get s i <= '\032')
        || String.unsafe_get s i = '\127'
      then
        ()
      else
        Buffer.add_char result (String.unsafe_get s i)
    done ;
    Buffer.contents result

  let base64encode s = modify Base64.encode_string s

  let base64decode s =
    let decode x = Base64.decode_exn (sanitize x) in
    modify decode s

  let writer = I.writer

  let wsframe s =
    modify
      (fun s ->
        let l = String.length s in
        if l < 126 then
          Printf.sprintf "%c%c%s" (char_of_int 0x82) (char_of_int l) s
        else if l < 65535 then
          Printf.sprintf "%c%c%s%s" (char_of_int 0x82) (char_of_int 126)
            (Helpers.marshal_int16 l) s
        else
          Printf.sprintf "%c%c%s%s" (char_of_int 0x82) (char_of_int 127)
            (Helpers.marshal_int32 (Int32.of_int l))
            s
      )
      s

  let wsframe_old s = modify (fun s -> Printf.sprintf "\x00%s\xff" s) s

  let ( let* ) = ( >>= )

  let rec wsunframe it =
    let read_sz =
      let* sz = read_int8 in
      return (sz >= 128, sz land 0x7f)
    in
    let read_size sz =
      if sz < 126 then
        return sz
      else if sz = 126 then
        read_int16
      else (* sz = 127 *)
        let* x = read_int32 in
        return (Int32.to_int x)
    in
    let read_mask has_mask =
      if has_mask then
        readn 4
      else
        return "\x00\x00\x00\x00"
    in
    let rec inner acc = function
      | IE_cont (None, k) as it ->
          let* op = read_int8 in
          let* has_mask, sz = read_sz in
          let* size = read_size sz in
          let* mask = read_mask has_mask in
          let* payload = readn size in
          let unmasked = Helpers.unmask mask payload in
          if op land 0x0f = 0x08 then (* close frame *)
            return it
          else if not (op land 0x80 = 0x80) then
            inner (acc ^ unmasked) it
          else
            let ( let@ ) = IO.bind in
            (let@ it', _ = k (Chunk (acc ^ unmasked)) in
             IO.return (wsunframe it')
            )
            |> liftI
      | it ->
          return it
    in
    inner "" it

  let rec wsunframe_old = function
    | IE_cont (None, k) ->
        let* _ = heads "\x00" in
        let* str = break (( = ) '\xff') in
        let* () = drop 1 in
        let ( let@ ) = IO.bind in
        (let@ it', _ = k (Chunk str) in
         IO.return (wsunframe_old it')
        )
        |> liftI
    | it ->
        return it
end
