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

open Rresult
module Pem = Gencertlib.Pem

type uuid = Uuidm.t

type paths = {host_cert_path: string; ca_certs_dir: string; ca_crls_dir: string}

type host = {
    private_key: X509.Private_key.t
  ; cert: X509.Certificate.t
  ; intermediates: X509.Certificate.t list
}

type ca = {cert: X509.Certificate.t; uuid: uuid; mutable deleted: bool}

type crl = {name: string; cert: X509.Certificate.t; mutable deleted: bool}

type t = {
    host: host option
  ; mutable ca: ca list
  ; mutable crl: crl list
  ; paths: paths
}

let ( let* ) = R.bind

let defer f = Fun.protect ~finally:f

let default_paths () =
  {
    host_cert_path= !Xapi_globs.server_cert_path
  ; ca_certs_dir= "/etc/stunnel/certs"
  ; ca_crls_dir= Stunnel.crl_path
  }

(* local, private, mutable state *)
let t : t option ref = ref None

(* calls in this API are protected by this mutex *)
let mx = Mutex.create ()

let with_mutex f =
  Mutex.lock mx ;
  defer (fun () -> Mutex.unlock mx) f

let get_unsafe () =
  match !t with
  | Some t ->
      R.ok t
  | None ->
      R.error_msgf "Certificate store has not been initialised %s" __LOC__

let has_extension ext path = Filename.extension path = ext

let filter_dir accept path =
  Sys.readdir path
  |> Array.to_list
  |> List.filter accept
  |> List.map (Filename.concat path)

let decode_key pem = Cstruct.of_string pem |> X509.Private_key.decode_pem

let decode_cert pem = Cstruct.of_string pem |> X509.Certificate.decode_pem

let decode_multi pem =
  Cstruct.of_string pem |> X509.Certificate.decode_pem_multiple

let read_host path =
  match Sys.file_exists path with
  | true ->
      let* pems =
        Pem.parse_file_using Pem.host_pem path |> function
        | Error msg ->
            R.error_msg msg
        | Ok _ as ok ->
            ok
      in
      let* private_key = decode_key pems.Pem.private_key in
      let* cert = decode_cert pems.Pem.host_cert in
      let* intermediates =
        decode_multi (String.concat "\n" pems.Pem.other_certs)
      in
      Ok (Some {private_key; cert; intermediates})
  | false ->
      Ok None

let read_crl path =
  let name = Filename.basename path in
  let* pem =
    Pem.parse_file_using Pem.cert path |> function
    | Error msg ->
        R.error_msg msg
    | Ok _ as ok ->
        ok
  in
  let* cert = decode_cert pem in
  Ok {cert; name; deleted= false}

let read_ca path =
  let name = Filename.basename path |> Filename.chop_extension in
  let* uuid =
    match Uuidm.of_string name with
    | Some uuid ->
        Ok uuid
    | None ->
        R.error_msgf "%s is not a uuid filename" path
  in
  let* pem =
    Pem.parse_file_using Pem.cert path |> function
    | Error msg ->
        R.error_msg msg
    | Ok _ as ok ->
        ok
  in
  let* cert = decode_cert pem in
  Ok {cert; uuid; deleted= false}

(* verrify that all values in a list are [Ok] and otherwise return
   the first [Error] value *)
let all_ok xs =
  let rec loop acc = function
    | [] ->
        Ok (List.rev acc)
    | Ok x :: xs ->
        loop (x :: acc) xs
    | (Error _ as e) :: _ ->
        e
  in
  loop [] xs

let read_crs_dir path =
  filter_dir (has_extension "*.pem") path |> List.map read_crl |> all_ok

let read_ca_dir path =
  filter_dir (has_extension "*.pem") path |> List.map read_ca |> all_ok

let init paths =
  with_mutex @@ fun () ->
  let* host = read_host paths.host_cert_path in
  let* crl = read_crs_dir paths.ca_crls_dir in
  let* ca = read_ca_dir paths.ca_certs_dir in
  t := Some {host; ca; crl; paths} ;
  Ok ()

let write_to path content =
  let perform () =
    let fd = Unix.(openfile path [O_WRONLY; O_TRUNC; O_CREAT] 0o400) in
    defer (fun () -> Unix.close fd) @@ fun () ->
    ignore @@ Unix.write_substring fd content 0 (String.length content)
  in
  R.trap_exn perform () |> R.error_exn_trap_to_msg

let sync_ca dir (ca : ca) =
  let name = Printf.sprintf "%s.pem" (Uuidm.to_string ca.uuid) in
  let path = Filename.concat dir name in
  let rm path = Sys.remove path in
  match ca.deleted with
  | true ->
      R.trap_exn rm path |> R.error_exn_trap_to_msg
  | false ->
      let pem = X509.Certificate.encode_pem ca.cert |> Cstruct.to_string in
      write_to path pem

let sync_crl dir (crl : crl) =
  let path = Filename.concat dir crl.name in
  let rm path = Sys.remove path in
  match crl.deleted with
  | true ->
      R.trap_exn rm path |> R.error_exn_trap_to_msg
  | false ->
      let pem = X509.Certificate.encode_pem crl.cert |> Cstruct.to_string in
      write_to path pem

let rec iter f = function
  | [] ->
      R.ok ()
  | x :: xs ->
      f x >>= fun () -> iter f xs

let rm_ca uuid () =
  with_mutex @@ fun () ->
  let* t = get_unsafe () in
  match List.find_opt (fun ca -> Uuidm.equal ca.uuid uuid) t.ca with
  | Some ca ->
      ca.deleted <- true ;
      R.ok ()
  | None ->
      R.error_msgf "CA cert %s does not exist" (Uuidm.to_string uuid)

let rm_crl name () =
  with_mutex @@ fun () ->
  let* t = get_unsafe () in
  match List.find_opt (fun crl -> crl.name = name) t.crl with
  | Some crl ->
      crl.deleted <- true ;
      R.ok ()
  | None ->
      R.error_msgf "crl cert %s does not exist" name

let sync () =
  with_mutex @@ fun () ->
  let* t = get_unsafe () in
  let* () = iter (sync_ca t.paths.ca_certs_dir) t.ca in
  let* () = iter (sync_crl t.paths.ca_crls_dir) t.crl in
  t.ca <- List.filter (fun (ca : ca) -> ca.deleted = false) t.ca ;
  t.crl <- List.filter (fun (crl : crl) -> crl.deleted = false) t.crl ;
  R.ok ()

let get_ca () =
  with_mutex @@ fun () ->
  let* t = get_unsafe () in
  let live (ca : ca) =
    match ca with {deleted= true; _} -> None | ca -> Some (ca.uuid, ca.cert)
  in
  List.filter_map live t.ca |> R.ok

let get_crl () =
  with_mutex @@ fun () ->
  let* t = get_unsafe () in
  let live (crl : crl) =
    match crl with
    | {deleted= true; _} ->
        None
    | crl ->
        Some (crl.name, crl.cert)
  in
  List.filter_map live t.crl |> R.ok

let get_host () =
  with_mutex @@ fun () ->
  let* t = get_unsafe () in
  match t.host with
  | None ->
      R.ok None
  | Some h ->
      R.ok (Some (h.private_key, h.cert, h.intermediates))

let add_ca uuid cert =
  with_mutex @@ fun () ->
  let* t = get_unsafe () in
  match List.find_opt (fun ca -> Uuidm.equal uuid ca.uuid) t.ca with
  | None ->
      let ca = {cert; uuid; deleted= false} in
      t.ca <- ca :: t.ca ;
      R.ok ()
  | Some _ ->
      R.error_msgf "CA %s already exists" (Uuidm.to_string uuid)

let add_crl name cert =
  with_mutex @@ fun () ->
  let* t = get_unsafe () in
  match List.find_opt (fun crl -> name = crl.name) t.crl with
  | None ->
      let crl = {cert; name; deleted= false} in
      t.crl <- crl :: t.crl ;
      R.ok ()
  | Some _ ->
      R.error_msgf "crl %s already exists" name
