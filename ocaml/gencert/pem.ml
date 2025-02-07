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

let is_data = function '-' -> false | _ -> true

let is_eol = function '\n' | '\r' -> true | _ -> false

let data = take_while1 is_data

type kind = RSA | EC | OTHER

type block = Key of string | Cert of string

let fail_fmt fmt = Printf.ksprintf (fun str -> fail str) fmt

let kind =
  string " RSA " *> return RSA
  <|> string " EC " *> return EC
  <|> string " " *> return OTHER

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
  string "-----BEGIN" *> kind <* string "PRIVATE KEY-----" <?> "key_header"

let key_footer k = string (footer k) <?> "key_footer"

let cert_header = string "-----BEGIN CERTIFICATE-----" <?> "cert_header"

let cert_footer = string "-----END CERTIFICATE-----" <?> "cert_footer"

let key =
  key_header >>= fun kind ->
  data >>= fun body ->
  key_footer kind *> return (String.concat "" [header kind; body; footer kind])
  <?> "key"

let cert =
  cert_header >>= fun hd ->
  data >>= fun body ->
  cert_footer >>= fun tl -> return (String.concat "" [hd; body; tl]) <?> "cert"

let line = take_till is_eol *> end_of_line

let any_block =
  cert >>= (fun c -> return (Cert c)) <|> (key >>= fun k -> return (Key k))

(* this skips over junk until we succeed finding the next block *)
let block = fix (fun m -> any_block <|> line *> m) <?> "until_block"

(* collect and tag all blocks *)
let blocks = many block <?> "PEM blocks"

(* decompose blocks into certs and keys *)
let pem =
  let ( let* ) = ( >>= ) in
  let strip = function Cert c -> c | Key k -> k in
  blocks >>= fun bs ->
  match List.partition (function Key _ -> true | Cert _ -> false) bs with
  | [Key k], Cert c :: xs ->
      return {private_key= k; host_cert= c; other_certs= List.map strip xs}
  | [_], [] ->
      let* p = pos in
      fail_fmt "PEM is lacking a certificate (at offset %d)" p
  | [], _ ->
      let* p = pos in
      fail_fmt "PEM is missing a private key (at offset %d)" p
  | _ :: _, _ ->
      let* p = pos in
      fail_fmt "PEM has more than one private key (at offset %d)" p

let defer f = Fun.protect ~finally:f

let read_file path =
  let ic = open_in path in
  defer (fun () -> close_in ic) @@ fun () ->
  really_input_string ic (in_channel_length ic)

let _parse_with t path =
  let consume = Consume.Prefix in
  read_file path |> parse_string ~consume t

let parse_string str =
  let consume = Consume.Prefix in
  parse_string ~consume pem str

let parse_file path =
  try read_file path |> parse_string
  with e ->
    Error (Printf.sprintf "Can't process %s: %s" path (Printexc.to_string e))
