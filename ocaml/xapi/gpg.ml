(*
 * Copyright (C) 2006-2022 Citrix Systems Inc.
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
(** Wrapper around gpg *)

open Xapi_stdext_std.Xstringext
open Xapi_stdext_pervasives.Pervasiveext
module Unixext = Xapi_stdext_unix.Unixext

module D = Debug.Make (struct let name = "gpg" end)

open D

let gpg_binary_path = "/usr/bin/gpg"

exception InvalidSignature

let parse_gpg_status status_data =
  let lines = String.split '\n' status_data in
  let status_contains substr =
    List.exists (fun s -> String.startswith substr s) lines
  in
  if
    not
      (status_contains "[GNUPG:] GOODSIG"
      && status_contains "[GNUPG:] TRUST_ULTIMATE"
      )
  then
    raise InvalidSignature ;
  let validsig = "[GNUPG:] VALIDSIG" in
  if status_contains validsig then
    let validsigline =
      List.find (fun s -> String.startswith validsig s) lines
    in
    match String.split ' ' validsigline with
    | _ :: _ :: fingerprint :: _ ->
        Some fingerprint
    | _ ->
        None
  else
    None

let simple_checksum file = Digest.to_hex (Digest.file file)

let common ty filename signature size f =
  let tmp_file, tmp_oc = Filename.open_temp_file ~mode:[Open_binary] "gpg" "" in
  let result_in = Unix.descr_of_out_channel tmp_oc in
  let result_out = Unix.openfile tmp_file [Unix.O_RDONLY] 0o0 in
  Unix.unlink tmp_file ;
  (* no need to close the 'tmp_oc' -> closing the fd is enough *)
  let status_out, status_in = Unix.pipe () in
  let status_in_uuid = Uuidx.to_string (Uuidx.make ()) in
  (* from the parent's PoV *)
  let fds_to_close = ref [result_out; result_in; status_out; status_in] in
  let close' fd =
    if List.mem fd !fds_to_close then (
      Unix.close fd ;
      fds_to_close := List.filter (fun x -> x <> fd) !fds_to_close
    )
  in
  let gpg_pub_keyring = Filename.concat !Xapi_globs.gpg_homedir "pubring.gpg" in
  let gpg_args =
    match ty with
    | `signed_cleartext ->
        [
          "--homedir"
        ; !Xapi_globs.gpg_homedir
        ; "--no-default-keyring"
        ; "--keyring"
        ; gpg_pub_keyring
        ; "--status-fd"
        ; status_in_uuid
        ; "--decrypt"
        ; filename
        ]
    | `detached_signature ->
        [
          filename
        ; Int64.to_string size
        ; "--homedir"
        ; !Xapi_globs.gpg_homedir
        ; "--no-default-keyring"
        ; "--keyring"
        ; gpg_pub_keyring
        ; "--status-fd"
        ; status_in_uuid
        ; "--verify"
        ; signature
        ]
    | `verified_signature ->
        [
          "--homedir"
        ; !Xapi_globs.gpg_homedir
        ; "--no-default-keyring"
        ; "--keyring"
        ; gpg_pub_keyring
        ; "--status-fd"
        ; status_in_uuid
        ; "--verify"
        ; signature
        ; filename
        ]
  in
  finally (* make sure I close all my open fds in the end *)
    (fun () ->
      (* Capture stderr output for logging *)
      match
        Forkhelpers.with_logfile_fd "gpg" (fun log_fd ->
            let pid =
              Forkhelpers.safe_close_and_exec None (Some result_in) (Some log_fd)
                [(status_in_uuid, status_in)]
                gpg_binary_path gpg_args
            in
            (* parent *)
            List.iter close' [result_in; status_in] ;
            finally (* always waitpid eventually *)
              (fun () ->
                let gpg_status = Unixext.string_of_fd status_out in
                let fingerprint = parse_gpg_status gpg_status in
                f fingerprint result_out
              )
              (fun () -> Forkhelpers.waitpid_fail_if_bad_exit pid)
        )
      with
      | Forkhelpers.Success (_, x) ->
          debug "gpg subprocess succeeded" ;
          x
      | Forkhelpers.Failure (_, Forkhelpers.Subprocess_failed 2) ->
          (* Happens when gpg cannot find a readable signature *)
          raise InvalidSignature
      | Forkhelpers.Failure (log, exn) ->
          debug "Error from gpg: %s" log ;
          raise exn
    )
    (fun () -> List.iter Unix.close !fds_to_close)

let with_signed_cleartext filename f =
  common `signed_cleartext filename "" Int64.zero f

let with_detached_signature filename signature size f =
  common `detached_signature filename signature size f

let with_verified_signature filename signature f =
  common `verified_signature filename signature Int64.zero f

let assert_name_is_valid ~name =
  let safe_char = function
    | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '.' | '_' | '-' ->
        true
    | _ ->
        false
  in
  let len = String.length name in
  let starts_with_dot = String.starts_with ~prefix:"." name in
  let is_valid = String.for_all safe_char name && not starts_with_dot in
  match (is_valid, len <= 0, len > 64) with
  | false, _, _ ->
      raise Api_errors.(Server_error (gpg_key_name_is_invalid, [name]))
  | true, true, _ ->
      raise Api_errors.(Server_error (gpg_key_name_is_empty, []))
  | true, false, true ->
      raise Api_errors.(Server_error (gpg_key_name_is_too_long, [name]))
  | true, false, false ->
      ()

module PubKeyMetaData = struct
  type t = {created: float; fingerprint: string}
end

let parse_pubkey_metadata md_str_in_colons =
  let open Angstrom in
  let single_field =
    take_while (fun x -> x <> ':') >>= fun v ->
    char ':' >>= fun _ -> return v
  in
  let pickup =
    single_field >>= fun _ ->
    count 4 single_field >>= fun _ ->
    single_field >>= fun created ->
    count 13 single_field >>= fun _ ->
    single_field >>= fun fingerprint -> return (created, fingerprint)
  in
  let consume = Consume.Prefix in
  match (parse_string ~consume pickup) md_str_in_colons with
  | Ok (date_str, fingerprint) ->
      let created =
        try Float.of_string date_str
        with _ ->
          error "Failed to parse GPG public key: invalid creation date: %s"
            date_str ;
          raise Api_errors.(Server_error (gpg_key_is_invalid, []))
      in
      let safe_char = function 'A' .. 'Z' | '0' .. '9' -> true | _ -> false in
      if not (String.for_all safe_char fingerprint) then (
        error "Failed to parse GPG public key: invalid fingerprint: %s"
          fingerprint ;
        raise Api_errors.(Server_error (gpg_key_is_invalid, []))
      ) ;
      PubKeyMetaData.{created; fingerprint}
  | Error msg ->
      error "Failed to parse GPG public key from a string: %s" msg ;
      raise Api_errors.(Server_error (gpg_key_is_invalid, []))

let parse_pubkey ~pubkey =
  let tmp_file = Filename.temp_file "gpg_pubkey" ".tmp" in
  Xapi_stdext_pervasives.Pervasiveext.finally
    (fun () ->
      try
        Unixext.write_string_to_file tmp_file pubkey ;
        let params =
          [
            "--homedir=/tmp"
          ; "--dry-run"
          ; "--with-fingerprint"
          ; "--with-colons"
          ; tmp_file
          ]
        in
        Helpers.call_script !Xapi_globs.gpg_cmd params |> parse_pubkey_metadata
      with e ->
        error "Failed to parse GPG public key: %s" (ExnHelper.string_of_exn e) ;
        raise Api_errors.(Server_error (gpg_key_is_invalid, []))
    )
    (fun () -> Unixext.unlink_safe tmp_file)

let create_db_record ~__context ~name ~created ~fingerprint ~_type =
  let ref = Ref.make () in
  let uuid = Uuidx.(to_string (make ())) in
  Db.Gpg_key.create ~__context ~ref ~uuid ~name
    ~created:(Xapi_stdext_date.Date.of_float created)
    ~fingerprint ~uninstalled:false ~_type ;
  ref
