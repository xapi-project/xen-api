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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Lesser General Public License for more details.
 *)

open Angstrom

type t = {private_key: string; host_cert: string; other_certs: string list}

let is_whitespace = function
  | ' ' ->
      true
  | '\t' ->
      true
  | '\n' ->
      true
  | '\r' ->
      true
  | _ ->
      false

let is_newline = function '\n' -> true | '\r' -> true | _ -> false

let is_data = function '-' -> false | _ -> true

let newline = take_while1 is_newline

let ws = take_while is_whitespace

let data = take_while1 is_data

let key_header = string "-----BEGIN PRIVATE KEY-----"

let key_footer = string "-----END PRIVATE KEY-----"

let cert_header = string "-----BEGIN CERTIFICATE-----"

let cert_footer = string "-----END CERTIFICATE-----"

let key =
  ws *> key_header >>= fun hd ->
  data >>= fun body ->
  key_footer >>= fun tl -> ws *> return (String.concat "" [hd; body; tl])

let cert =
  ws *> cert_header >>= fun hd ->
  data >>= fun body ->
  cert_footer >>= fun tl -> ws *> return (String.concat "" [hd; body; tl])

let host_pem =
  key >>= fun private_key ->
  cert >>= fun host_cert ->
  many cert >>= fun other_certs -> return {private_key; host_cert; other_certs}

let defer f = Fun.protect ~finally:f

let read_file path =
  let ic = open_in path in
  defer (fun () -> close_in ic) @@ fun () ->
  really_input_string ic (in_channel_length ic)

let parse_string_using parser str =
  let consume = Consume.Prefix in
  parse_string ~consume parser str

let parse_file_using parser path =
  try read_file path |> parse_string_using parser
  with e ->
    Error (Printf.sprintf "Can't process %s: %s" path (Printexc.to_string e))

let parse_string = parse_string_using host_pem

let parse_file = parse_file_using host_pem
