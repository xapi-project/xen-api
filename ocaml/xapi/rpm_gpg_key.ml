(*
 * Copyright (C) 2022 Citrix Systems Inc.
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

module Unixext = Xapi_stdext_unix.Unixext

module D = Debug.Make (struct let name = "rpm_gpg_key" end)

open D
open Gpg

let get_pubkey ~name =
  let file_path_name = Filename.concat !Xapi_globs.rpm_gpgkey_dir name in
  match Sys.file_exists file_path_name with
  | true -> (
    try
      let ic = open_in file_path_name in
      Fun.protect ~finally:(fun () -> close_in ic) @@ fun () ->
      let size = in_channel_length ic in
      Some (really_input_string ic size)
    with e ->
      error "Can't open the RPM GPG key file %s: %s" file_path_name
        (ExnHelper.string_of_exn e) ;
      None
  )
  | false ->
      error "The RPM GPG public key file %s doesn't exist" file_path_name ;
      None

let get_rpm_pubkey_string ~__context ~self =
  let name = Db.Gpg_key.get_name ~__context ~self in
  match Db.Gpg_key.get_type ~__context ~self with
  | `rpm_pubkey -> (
    match get_pubkey ~name with
    | Some pubkey ->
        pubkey
    | None ->
        raise (Api_errors.Server_error (Api_errors.gpg_key_is_invalid, [name]))
  )
  | _ ->
      error "The GPG key %s is not an RPM GPG public key." name ;
      raise (Api_errors.Server_error (Api_errors.gpg_key_is_invalid, [name]))

let import name =
  try
    let file_path_name = Filename.concat !Xapi_globs.rpm_gpgkey_dir name in
    Helpers.call_script !Xapi_globs.rpm_cmd ["--import"; file_path_name]
    |> ignore
  with e ->
    error "Can't import RPM GPG key: %s" (ExnHelper.string_of_exn e) ;
    raise Api_errors.(Server_error (rpm_gpg_key_can_not_be_installed, [name]))

let get_imported_rpm_gpgkey_names () =
  Helpers.call_script !Xapi_globs.rpm_cmd ["-qa"; "gpg-pubkey"]
  |> Astring.String.cuts ~sep:"\n"
  |> List.filter (fun s -> Astring.String.is_prefix ~affix:"gpg-pubkey-" s)

let extract_pubkey_from_rpm ~name str =
  let open Angstrom in
  let is_eol = function '\n' -> true | _ -> false in
  let rec line_in_key acc () =
    take_till is_eol <* end_of_line >>= function
    | "-----END PGP PUBLIC KEY BLOCK-----" as footer ->
        return (footer :: acc)
    | l ->
        line_in_key (l :: acc) ()
  in
  let rec line () =
    take_till is_eol <* end_of_line >>= function
    | "-----BEGIN PGP PUBLIC KEY BLOCK-----" as header ->
        line_in_key [header] ()
    | _ ->
        line ()
  in
  let consume = Consume.Prefix in
  let r = (parse_string ~consume (line ())) str in
  match r with
  | Ok reversed_lines ->
      let lines = List.rev reversed_lines in
      debug "Extracted pubkey:" ;
      List.iter (fun l -> debug "%s" l) lines ;
      Some (String.concat "\n" lines)
  | Error msg ->
      error "Failed to parse RPM GPG key '%s': %s" name msg ;
      None

let install_rpmgpgkey ~name ~pubkey ~fingerprint =
  (* Check with the existing key files which are of same name *)
  get_pubkey ~name
  |> Option.map (fun pubkey ->
         match parse_pubkey ~pubkey with
         | PubKeyMetaData.{fingerprint= fp; _} when fingerprint = fp ->
             debug "A same RPM GPG key (with fingerprint %s) exists already"
               fingerprint ;
             (* Importing is idempotent *)
             import name
         | _ ->
             raise
               Api_errors.(
                 Server_error (rpm_gpg_key_with_same_name_already_exists, [name])
               )
         | exception e ->
             error
               "Another RPM GPG key with same name (%s) exists already, but it \
                can't be parsed: %s"
               name
               (ExnHelper.string_of_exn e) ;
             raise
               Api_errors.(
                 Server_error (rpm_gpg_key_with_same_name_already_exists, [name])
               )
     )
  |> function
  | None -> (
      let file_path_name = Filename.concat !Xapi_globs.rpm_gpgkey_dir name in
      try
        Unixext.write_string_to_file file_path_name pubkey ;
        (* The key may be imported already. But importing is idempotent *)
        import name
      with e ->
        Unixext.unlink_safe file_path_name ;
        error "Can't intall the RPM GPG key through the file %s: %s"
          file_path_name
          (ExnHelper.string_of_exn e) ;
        raise
          Api_errors.(Server_error (rpm_gpg_key_can_not_be_installed, [name]))
    )
  | Some _ ->
      ()

let uninstall_rpmgpgkey ~name ~fingerprint =
  let file_path_name = Filename.concat !Xapi_globs.rpm_gpgkey_dir name in
  debug "uninstall_rpmgpgkey name=%s fingerprint=%s" name fingerprint ;
  Unixext.unlink_safe file_path_name ;
  (* Remove the key from RPM with best-effort.
   * List all RPM GPG public keys firstly, extract the pubkey from each key, and
   * find the key by comparing the fingerprint.
   *)
  get_imported_rpm_gpgkey_names ()
  |> List.iter (fun pubkey_name ->
         Helpers.call_script !Xapi_globs.rpm_cmd ["-qi"; pubkey_name]
         |> extract_pubkey_from_rpm ~name:pubkey_name
         |> Option.iter (fun pubkey ->
                match parse_pubkey ~pubkey with
                | PubKeyMetaData.{fingerprint= fp; _} when fingerprint = fp ->
                    Helpers.call_script !Xapi_globs.rpm_cmd ["-e"; pubkey_name]
                    |> ignore
                | _ ->
                    ()
                | exception e ->
                    error
                      "Failed to check imported RPM pubkey %s in \
                       uninstall_rpmgpgkey: %s"
                      pubkey_name
                      (ExnHelper.string_of_exn e)
            )
     )
