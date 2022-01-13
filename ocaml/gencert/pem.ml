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

let is_data = function '-' -> false | _ -> true

let ws = take_while is_whitespace

let data = take_while1 is_data

type kind = RSA | EC | OTHER

let kind = option OTHER (string "RSA" *> return RSA <|> string "EC" *> return EC)

let header = function
  | RSA ->
      "-----BEGIN RSA PRIVATE KEY-----"
  | EC ->
      "-----BEGIN EC PRIVATE KEY-----"
  | OTHER ->
      "-----BEGIN PRIVATE KEY-----"

let footer = function
  | RSA ->
      "-----END RSA PRIVATE KEY-----"
  | EC ->
      "-----END EC PRIVATE KEY-----"
  | OTHER ->
      "-----END PRIVATE KEY-----"

let key_header =
  string "-----BEGIN" *> ws *> kind <* ws <* string "PRIVATE KEY-----"

let key_footer k = string (footer k)

let cert_header = string "-----BEGIN CERTIFICATE-----"

let cert_footer = string "-----END CERTIFICATE-----"

let key =
  ws *> key_header >>= fun k ->
  data >>= fun body ->
  key_footer k *> ws *> return (String.concat "" [header k; body; footer k])

let cert =
  ws *> cert_header >>= fun hd ->
  data >>= fun body ->
  cert_footer >>= fun tl -> ws *> return (String.concat "" [hd; body; tl])

let pem =
  key >>= fun private_key ->
  cert >>= fun host_cert ->
  many cert >>= fun other_certs -> return {private_key; host_cert; other_certs}

let defer f = Fun.protect ~finally:f

let read_file path =
  let ic = open_in path in
  defer (fun () -> close_in ic) @@ fun () ->
  really_input_string ic (in_channel_length ic)

let parse_string str =
  let consume = Consume.Prefix in
  parse_string ~consume pem str

let parse_file path =
  try read_file path |> parse_string
  with e ->
    Error (Printf.sprintf "Can't process %s: %s" path (Printexc.to_string e))
